module c3lox;

import std::io;

enum Precedence : Uint8 {
	NONE,
	ASSIGNMENT, // =
	OR,         // or
	AND,        // and
	EQUALITY,   // == !=
	COMPARISON, // < > <= >=
	TERM,       // + -
	FACTOR,     // * /
	UNARY,      // ! -
	CALL,       // . ()
	PRIMARY
}

struct ParseRule {
	ParseFn prefix;
	ParseFn infix;
	Precedence precedence;
}

struct Parser {
	LoxToken current;
	LoxToken previous;
	bool had_error;
	bool panic_mode;
}

struct Local {
	LoxToken name;
	sz depth;
}

struct Compiler {
	Local[UINT8_COUNT] locals;
	sz locals_count;
	sz scope_depth;

	LoxScanner scanner;
	Parser parser;
	Chunk* chunk;
}

fn void Compiler.init(Compiler* compiler, char[] source) {
	compiler.scanner.init(source);
	compiler.locals_count = 0;
	compiler.scope_depth = 0;
}

fn void Compiler.error_at(Compiler* compiler, LoxToken* token, String message) {
	if (compiler.parser.panic_mode) return;
	compiler.parser.panic_mode = true;
	io::eprintf("[line %d] Error", token.line);

	if (token.type == EOF) {
		io::eprintf(" at end");
	} else if (token.type == ERROR) {
		// Nothing.
	} else {
		io::eprintf(" at '%s'", token.lexeme);
	}

	io::eprintfn(": %s", message);
	compiler.parser.had_error = true;
}

fn void Compiler.error(Compiler* compiler, String message) {
	compiler.error_at(&compiler.parser.previous, message);
}

fn void Compiler.error_at_current(Compiler* compiler, String message) {
	compiler.error_at(&compiler.parser.current, message);
}

fn void Compiler.advance(Compiler* compiler) {
	compiler.parser.previous = compiler.parser.current;

	for (;;) {
		compiler.parser.current = compiler.scanner.scan_token();
		if (compiler.parser.current.type != ERROR) break;
		compiler.error_at_current(compiler.parser.current.lexeme);
	}
}

fn void Compiler.consume(Compiler* compiler, TokenType type, String message) {
	if (compiler.parser.current.type == type) {
		compiler.advance();
		return;
	}

	compiler.error_at_current(message);
}

fn bool Compiler.check(Compiler* compiler, TokenType type) {
	return compiler.parser.current.type == type;
}

fn bool Compiler.match(Compiler* compiler, TokenType type) {
	if (!compiler.check(type)) return false;
	compiler.advance();
	return true;
}

fn void Compiler.emit_byte(Compiler* compiler, Uint8 byte) {
	compiler.chunk.write(byte, compiler.parser.previous.line);
}

fn void Compiler.emit_return(Compiler* compiler) => compiler.emit_byte(Op.RETURN);

fn ParseRule* Compiler.get_rule(Compiler* compiler, TokenType type) => RULE[type];

fn void Compiler.emit_bytes(Compiler* compiler, Uint8 byte1, Uint8 byte2) {
	compiler.emit_byte(byte1);
	compiler.emit_byte(byte2);
}

fn void Compiler.end(Compiler* compiler) {
	compiler.emit_return();
	$if $feat(DEBUG_PRINT_CODE):
		if (!compiler.parser.had_error) compiler.chunk.disassemble_chunk("code");
	$endif
}

fn void Compiler.begin_scope(Compiler* compiler) {
	compiler.scope_depth++;
}

fn void Compiler.end_scope(Compiler* compiler) {
	compiler.scope_depth--;
	while (compiler.locals_count > 0
		&& compiler.locals[compiler.locals_count - 1].depth > compiler.scope_depth) {
		compiler.emit_byte(Op.POP);
		compiler.locals_count--;
	}
}

fn void number(Vm* vm, bool can_assign) {
	double val = vm.compiler.parser.previous.lexeme.to_double()!!;
	vm.compiler.emit_constant(number_val(val));
}

fn void string(Vm* vm, bool can_assign) {
	String inner = vm.compiler.parser.previous.lexeme[1..^2];
	Obj* c       = vm.allocate_string(inner);
	vm.compiler.emit_constant(obj_val(c));
}

fn void named_variable(Vm* vm, LoxToken name, bool can_assign) {
	Uint8 get_op, set_op;
	sz arg = vm.compiler.resolve_local(&name);
	if (arg != -1) {
		get_op = Op.GET_LOCAL;
		set_op = Op.SET_LOCAL;
	} else {
		arg = identifier_constant(vm, &name);
		get_op = Op.GET_GLOBAL;
		set_op = Op.SET_GLOBAL;
	}

	if (can_assign && vm.compiler.match(EQUAL)) {
		expression(vm);
		vm.compiler.emit_bytes(set_op, (Uint8)arg);
	} else {
		vm.compiler.emit_bytes(get_op, (Uint8)arg);
	}
}

fn void variable(Vm* vm, bool can_assign) {
	named_variable(vm, vm.compiler.parser.previous, can_assign);
}

fn void parse_precedence(Vm* vm, Precedence precedence) {
	vm.compiler.advance();
	ParseFn prefix_rule = vm.compiler.get_rule(vm.compiler.parser.previous.type).prefix;
	if (prefix_rule == null) {
		vm.compiler.error("Expect expression.");
		return;
	}

	bool can_assign = precedence <= ASSIGNMENT;
	prefix_rule(vm, can_assign);

	while (precedence <= vm.compiler.get_rule(vm.compiler.parser.current.type).precedence) {
		vm.compiler.advance();
		ParseFn infix_rule = vm.compiler.get_rule(vm.compiler.parser.previous.type).infix;
		infix_rule(vm, can_assign);
	}

	if (can_assign && vm.compiler.match(EQUAL)) {
		vm.compiler.error("Invalid assignment target.");
	}
}

fn Uint8 identifier_constant(Vm* vm, LoxToken* name) {
	Uint8? val = vm.compiler.chunk.globals.get(name.lexeme);
	if (try val) return val;

	Uint8 idx = vm.compiler.make_constant(obj_val(vm.allocate_ident(name.lexeme)));
	vm.compiler.chunk.globals.set(name.lexeme, idx);
	return idx;
}

fn bool identifiers_equal(LoxToken* a, LoxToken* b) {
	if (a.lexeme.len != b.lexeme.len) return false;
	return (a.lexeme == b.lexeme);
}

fn sz Compiler.resolve_local(Compiler* compiler, LoxToken* name) {
	for (sz i = compiler.locals_count - 1; i >= 0; i--) {
		Local* local = &compiler.locals[i];
		if (identifiers_equal(name, &local.name)) {
			if (local.depth == -1) {
				compiler.error("Can't read local variable in its own initializer.");
			}
		}
		return i;
	}
	return -1;
}

fn void Compiler.add_local(Compiler* compiler, LoxToken name) {
	if (compiler.locals_count == UINT8_COUNT) {
		compiler.error("Too man local variables in functions");
		return;
	}
	Local* local = &compiler.locals[compiler.locals_count++];
	local.name = name;
	local.depth = -1;
}

fn void Compiler.declare_variable(Compiler* compiler) {
	if (compiler.scope_depth == 0) return;

	LoxToken* name = &compiler.parser.previous;
	for (sz i = compiler.locals_count - 1; i >= 0; i--) {
		Local* local = &compiler.locals[i];
		if (local.depth != -1 && local.depth < compiler.scope_depth) {
			break;
		}

		if (identifiers_equal(name, &local.name)) {
			compiler.error("Already a variable with this name in this scope.");
		}
	}
	compiler.add_local(*name);
}

fn Uint8 parse_variable(Vm* vm, String error_message) {
	vm.compiler.consume(IDENTIFIER, error_message);

	vm.compiler.declare_variable();
	if (vm.compiler.scope_depth > 0) return 0;

	return identifier_constant(vm, &vm.compiler.parser.previous);
}

fn void Compiler.mark_initialized(Compiler* compiler) {
	compiler.locals[compiler.locals_count - 1].depth = compiler.scope_depth;
}

fn void Compiler.define_variable(Compiler* compiler, Uint8 global) {
	if (compiler.scope_depth > 0) {
		compiler.mark_initialized();
		return;
	}
	compiler.emit_bytes(Op.DEFINE_GLOBAL, global);
}

fn void unary(Vm* vm, bool can_assign) {
	TokenType operator_type = vm.compiler.parser.previous.type;
	parse_precedence(vm, UNARY);

	switch (operator_type) {
		case BANG: vm.compiler.emit_byte(Op.NOT);
		case MINUS: vm.compiler.emit_byte(Op.NEGATE);
		default: return;
	}
}

fn void binary(Vm* vm, bool can_assign) {
	TokenType operator_type = vm.compiler.parser.previous.type;
	ParseRule* rule         = vm.compiler.get_rule(operator_type);
	parse_precedence(vm, (Precedence)(rule.precedence.ordinal + 1));

	// c3fmt off
	switch (operator_type) {
		case BANG_EQUAL:    vm.compiler.emit_bytes(Op.EQUAL, Op.NOT);
		case EQUAL_EQUAL:   vm.compiler.emit_byte(Op.EQUAL);
		case GREATER:       vm.compiler.emit_byte(Op.GREATER);
		case GREATER_EQUAL: vm.compiler.emit_bytes(Op.LESS, Op.NOT);
		case LESS:          vm.compiler.emit_byte(Op.LESS);
		case LESS_EQUAL:    vm.compiler.emit_bytes(Op.GREATER, Op.NOT);
		case PLUS:          vm.compiler.emit_byte(Op.ADD);
		case MINUS:         vm.compiler.emit_byte(Op.SUBTRACT);
		case STAR:          vm.compiler.emit_byte(Op.MULTIPLY);
		case SLASH:         vm.compiler.emit_byte(Op.DIVIDE);
		default:            return;
	}
	// c3fmt on
}

fn void literal(Vm* vm, bool can_assign) {
	switch (vm.compiler.parser.previous.type) {
		case FALSE: vm.compiler.emit_byte(Op.FALSE); break;
		case NIL: vm.compiler.emit_byte(Op.NIL); break;
		case TRUE: vm.compiler.emit_byte(Op.TRUE); break;
		default: return;  // Unreachable.
	}
}

fn void expression(Vm* vm) {
	parse_precedence(vm, ASSIGNMENT);
}

fn void block(Vm* vm) {
	while (!vm.compiler.check(RIGHT_BRACE) && !vm.compiler.check(EOF)) {
		declaration(vm);
	}

	vm.compiler.consume(RIGHT_BRACE, "Expect '} after block");
}

fn void expression_statement(Vm* vm) {
	expression(vm);
	vm.compiler.consume(SEMICOLON, "Expect ';' after expression.");
	vm.compiler.emit_byte(Op.POP);
}

fn void var_declaration(Vm* vm) {
	Uint8 global = parse_variable(vm, "Expect variable name.");

	if (vm.compiler.match(EQUAL)) {
		expression(vm);
	} else {
		vm.compiler.emit_byte(Op.NIL);
	}
	vm.compiler.consume(SEMICOLON, "Expect ';' after variable declaration.");
	vm.compiler.define_variable(global);
}

fn void print_statement(Vm* vm) {
	expression(vm);
	vm.compiler.consume(SEMICOLON, "Expect ';' after value.");
	vm.compiler.emit_byte(Op.PRINT);
}

fn void Compiler.synchronize(Compiler* compiler) {
	compiler.parser.panic_mode = false;

	while (compiler.parser.current.type != EOF) {
		if (compiler.parser.previous.type == SEMICOLON) return;
		switch (compiler.parser.current.type) {
			case CLASS:
			case FUN:
			case VAR:
			case FOR:
			case IF:
			case WHILE:
			case PRINT:
			case RETURN: return;
			default:
		}

		compiler.advance();
	}
}
fn void declaration(Vm* vm) {
	if (vm.compiler.match(VAR)) {
		var_declaration(vm);
	} else {
		statement(vm);
	}
	if (vm.compiler.parser.panic_mode) vm.compiler.synchronize();
}

fn void statement(Vm* vm) {
	if (vm.compiler.match(PRINT)) {
		print_statement(vm);
	} else if (vm.compiler.match(LEFT_BRACE)) {
		vm.compiler.begin_scope();
		block(vm);
		vm.compiler.end_scope();
	} else {
		expression_statement(vm);
	}
}

fn void grouping(Vm* vm, bool can_assign) {
	expression(vm);
	vm.compiler.consume(RIGHT_PAREN, "Expect ')' after compiler_expression.");
}

fn Uint8 Compiler.make_constant(Compiler* compiler, Value value) {
	sz constant = compiler.chunk.add_constant(value);
	if (constant > UINT8_MAX) {
		compiler.error("Too many constants in one chunk.");
		return 0;
	}

	return (Uint8)constant;
}

fn void Compiler.emit_constant(Compiler* compiler, Value value) {
	compiler.emit_bytes(Op.CONSTANT, compiler.make_constant(value));
}

fn bool compile(Vm* vm, char[] source, Chunk* new_chunk) {
	vm.compiler.init(source);
	vm.compiler.chunk = new_chunk;

	vm.compiler.parser.had_error = false;
	vm.compiler.parser.panic_mode = false;

	vm.compiler.advance();
	while (!vm.compiler.match(EOF)) {
		declaration(vm);
	}
	vm.compiler.end();
	return !vm.compiler.parser.had_error;
}

// c3fmt off
const ParseRule*[*] RULE = {
	[TokenType.LEFT_PAREN]    = &&{ &grouping, null,    Precedence.NONE },
	[TokenType.RIGHT_PAREN]   = &&{ null,      null,    Precedence.NONE },
	[TokenType.LEFT_BRACE]    = &&{ null,      null,    Precedence.NONE },
	[TokenType.RIGHT_BRACE]   = &&{ null,      null,    Precedence.NONE },
	[TokenType.COMMA]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.DOT]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.MINUS]         = &&{ &unary,    &binary, Precedence.TERM },
	[TokenType.PLUS]          = &&{ null,      &binary, Precedence.TERM },
	[TokenType.SEMICOLON]     = &&{ null,      null,    Precedence.NONE },
	[TokenType.SLASH]         = &&{ null,      &binary, Precedence.FACTOR },
	[TokenType.STAR]          = &&{ null,      &binary, Precedence.FACTOR },
	[TokenType.BANG]          = &&{ &unary,    null,    Precedence.NONE },
	[TokenType.BANG_EQUAL]    = &&{ null,      &binary, Precedence.EQUALITY },
	[TokenType.EQUAL]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.EQUAL_EQUAL]   = &&{ null,      &binary, Precedence.EQUALITY },
	[TokenType.GREATER]       = &&{ null,      &binary, Precedence.COMPARISON },
	[TokenType.GREATER_EQUAL] = &&{ null,      &binary, Precedence.COMPARISON },
	[TokenType.LESS]          = &&{ null,      &binary, Precedence.COMPARISON },
	[TokenType.LESS_EQUAL]    = &&{ null,      &binary, Precedence.COMPARISON },
	[TokenType.IDENTIFIER]    = &&{ &variable, null,    Precedence.NONE },
	[TokenType.STRING]        = &&{ &string,   null,    Precedence.NONE },
	[TokenType.NUMBER]        = &&{ &number,   null,    Precedence.NONE },
	[TokenType.AND]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.CLASS]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.ELSE]          = &&{ null,      null,    Precedence.NONE },
	[TokenType.FALSE]         = &&{ &literal,  null,    Precedence.NONE },
	[TokenType.FOR]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.FUN]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.IF]            = &&{ null,      null,    Precedence.NONE },
	[TokenType.NIL]           = &&{ &literal,  null,    Precedence.NONE },
	[TokenType.OR]            = &&{ null,      null,    Precedence.NONE },
	[TokenType.PRINT]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.RETURN]        = &&{ null,      null,    Precedence.NONE },
	[TokenType.SUPER]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.THIS]          = &&{ null,      null,    Precedence.NONE },
	[TokenType.TRUE]          = &&{ &literal,  null,    Precedence.NONE },
	[TokenType.VAR]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.WHILE]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.ERROR]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.EOF]           = &&{ null,      null,    Precedence.NONE },
};
// c3fmt on

