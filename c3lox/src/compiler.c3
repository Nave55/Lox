module c3lox;

import std::io;
import std::collections::map;

enum Prec : Uint8 {
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
	Prec precedence;
}

// c3fmt off
const ParseRule*[*] RULE = {
	[TokenType.LEFT_PAREN]    = &&{ &grouping, null,    Prec.NONE },
	[TokenType.RIGHT_PAREN]   = &&{ null,      null,    Prec.NONE },
	[TokenType.LEFT_BRACE]    = &&{ null,      null,    Prec.NONE },
	[TokenType.RIGHT_BRACE]   = &&{ null,      null,    Prec.NONE },
	[TokenType.COMMA]         = &&{ null,      null,    Prec.NONE },
	[TokenType.DOT]           = &&{ null,      null,    Prec.NONE },
	[TokenType.MINUS]         = &&{ &unary,    &binary, Prec.TERM },
	[TokenType.PLUS]          = &&{ null,      &binary, Prec.TERM },
	[TokenType.SEMICOLON]     = &&{ null,      null,    Prec.NONE },
	[TokenType.SLASH]         = &&{ null,      &binary, Prec.FACTOR },
	[TokenType.STAR]          = &&{ null,      &binary, Prec.FACTOR },
	[TokenType.PERCENT]       = &&{ null,      &binary, Prec.FACTOR },
	[TokenType.BANG]          = &&{ &unary,    null,    Prec.NONE },
	[TokenType.BANG_EQUAL]    = &&{ null,      &binary, Prec.EQUALITY },
	[TokenType.EQUAL]         = &&{ null,      null,    Prec.NONE },
	[TokenType.EQUAL_EQUAL]   = &&{ null,      &binary, Prec.EQUALITY },
	[TokenType.GREATER]       = &&{ null,      &binary, Prec.COMPARISON },
	[TokenType.GREATER_EQUAL] = &&{ null,      &binary, Prec.COMPARISON },
	[TokenType.LESS]          = &&{ null,      &binary, Prec.COMPARISON },
	[TokenType.LESS_EQUAL]    = &&{ null,      &binary, Prec.COMPARISON },
	[TokenType.IDENTIFIER]    = &&{ &variable, null,    Prec.NONE },
	[TokenType.STRING]        = &&{ &string,   null,    Prec.NONE },
	[TokenType.NUMBER]        = &&{ &number,   null,    Prec.NONE },
	[TokenType.AND]           = &&{ null,      &and_,   Prec.AND },
	[TokenType.CLASS]         = &&{ null,      null,    Prec.NONE },
	[TokenType.ELSE]          = &&{ null,      null,    Prec.NONE },
	[TokenType.FALSE]         = &&{ &literal,  null,    Prec.NONE },
	[TokenType.FOR]           = &&{ null,      null,    Prec.NONE },
	[TokenType.FUN]           = &&{ null,      null,    Prec.NONE },
	[TokenType.IF]            = &&{ null,      null,    Prec.NONE },
	[TokenType.NIL]           = &&{ &literal,  null,    Prec.NONE },
	[TokenType.OR]            = &&{ null,      &or_,    Prec.OR },
	[TokenType.PRINT]         = &&{ null,      null,    Prec.NONE },
	[TokenType.RETURN]        = &&{ null,      null,    Prec.NONE },
	[TokenType.BREAK]         = &&{ null,      null,    Prec.NONE },
	[TokenType.CONTINUE]      = &&{ null,      null,    Prec.NONE },
	[TokenType.SUPER]         = &&{ null,      null,    Prec.NONE },
	[TokenType.THIS]          = &&{ null,      null,    Prec.NONE },
	[TokenType.TRUE]          = &&{ &literal,  null,    Prec.NONE },
	[TokenType.VAR]           = &&{ null,      null,    Prec.NONE },
	[TokenType.WHILE]         = &&{ null,      null,    Prec.NONE },
	[TokenType.ERROR]         = &&{ null,      null,    Prec.NONE },
	[TokenType.EOF]           = &&{ null,      null,    Prec.NONE },
};
// c3fmt on

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

struct Loop {
	sz[MAX_BREAKS] break_jumps;
	sz break_count;
	sz continue_target;
	sz scope_depth;
	Loop* enclosing;
}

struct Compiler {
	Local[UINT8_COUNT] locals;
	HashMap{String, Uint8} global_map;
	sz locals_count;
	sz scope_depth;

	LoxScanner scanner;
	Parser parser;
	Loop* current_loop;
}

fn void Compiler.init(Compiler* compiler, char[] source) {
	compiler.scanner.init(source);
	compiler.locals_count = 0;
	compiler.scope_depth = 0;
	compiler.current_loop = null;
}

fn void error_at(Vm* vm, LoxToken* token, String message) {
	if (vm.compiler.parser.panic_mode) return;
	vm.compiler.parser.panic_mode = true;
	io::eprintf("[line %d] Error", token.line);

	if (token.type == EOF) {
		io::eprintf(" at end");
	} else if (token.type == ERROR) {
		// Nothing.
	} else {
		io::eprintf(" at '%s'", token.lexeme);
	}

	io::eprintfn(": %s", message);
	vm.compiler.parser.had_error = true;
}

fn void error(Vm* vm, String message) {
	error_at(vm, &vm.compiler.parser.previous, message);
}

fn void error_at_current(Vm* vm, String message) {
	error_at(vm, &vm.compiler.parser.current, message);
}

fn void emit_byte(Vm* vm, Uint8 byte) {
	vm.chunk.write(byte, vm.compiler.parser.previous.line);
}

fn void emit_return(Vm* vm) => emit_byte(vm, Op.RETURN);

fn void emit_bytes(Vm* vm, Uint8 byte1, Uint8 byte2) {
	emit_byte(vm, byte1);
	emit_byte(vm, byte2);
}

fn void emit_loop(Vm* vm, sz loop_start) {
	emit_byte(vm, Op.LOOP);

	sz offset = vm.chunk.count() - loop_start + 2;
	if (offset > UINT16_MAX) error(vm, "Loop body too large.");

	emit_byte(vm, (Uint8)((offset >> 8) & 0xff));
	emit_byte(vm, (Uint8)(offset & 0xff));
}

fn sz emit_jump(Vm* vm, Uint8 instruction) {
	emit_byte(vm, instruction);
	emit_byte(vm, 0xff);
	emit_byte(vm, 0xff);
	return vm.chunk.count() - 2;
}

fn void advance(Vm* vm) {
	vm.compiler.parser.previous = vm.compiler.parser.current;

	for (;;) {
		vm.compiler.parser.current = vm.compiler.scanner.scan_token();
		if (vm.compiler.parser.current.type != ERROR) break;
		error_at_current(vm, vm.compiler.parser.current.lexeme);
	}
}

fn void consume(Vm* vm, TokenType type, String message) {
	if (vm.compiler.parser.current.type == type) {
		advance(vm);
		return;
	}

	error_at_current(vm, message);
}

fn bool check(Vm* vm, TokenType type) {
	return vm.compiler.parser.current.type == type;
}

fn bool match(Vm* vm, TokenType type) {
	if (!check(vm, type)) return false;
	advance(vm);
	return true;
}

fn ParseRule* get_rule(TokenType type) => RULE[type];

fn void parse_precedence(Vm* vm, Prec precedence) {
	advance(vm);
	ParseFn prefix_rule = get_rule(vm.compiler.parser.previous.type).prefix;
	if (prefix_rule == null) {
		error(vm, "Expect expression.");
		return;
	}

	bool can_assign = precedence <= ASSIGNMENT;
	prefix_rule(vm, can_assign);

	while (precedence <= get_rule(vm.compiler.parser.current.type).precedence) {
		advance(vm);
		ParseFn infix_rule = get_rule(vm.compiler.parser.previous.type).infix;
		infix_rule(vm, can_assign);
	}

	if (can_assign && match(vm, EQUAL)) {
		error(vm, "Invalid assignment target.");
	}
}

fn void expression(Vm* vm) {
	parse_precedence(vm, ASSIGNMENT);
}

fn Uint8 make_constant(Vm* vm, Value value) {
	sz constant = vm.chunk.add_constant(value);
	if (constant > UINT8_MAX) {
		error(vm, "Too many constants in one chunk.");
		return 0;
	}

	return (Uint8)constant;
}

fn Uint8 identifier_constant(Vm* vm, LoxToken* name) {
	Uint8? val = vm.compiler.global_map.get(name.lexeme);
	if (try val) return val;

	Uint8 idx = make_constant(vm, obj_val(vm.allocate_ident(name.lexeme)));
	vm.compiler.global_map.set(name.lexeme, idx);
	return idx;
}

fn void named_variable(Vm* vm, LoxToken name, bool can_assign) {
	Uint8 get_op, set_op;
	sz arg = resolve_local(vm, &name);
	if (arg != -1) {
		get_op = Op.GET_LOCAL;
		set_op = Op.SET_LOCAL;
	} else {
		arg = identifier_constant(vm, &name);
		get_op = Op.GET_GLOBAL;
		set_op = Op.SET_GLOBAL;
	}

	if (can_assign && match(vm, EQUAL)) {
		expression(vm);
		emit_bytes(vm, set_op, (Uint8)arg);
	} else if (can_assign && match(vm, PLUS_EQUAL)) {
		emit_bytes(vm, get_op, (Uint8)arg);
		expression(vm);
		emit_byte(vm, Op.ADD);
		emit_bytes(vm, set_op, (Uint8)arg);
	} else if (can_assign && match(vm, MINUS_EQUAL)) {
		emit_bytes(vm, get_op, (Uint8)arg);
		expression(vm);
		emit_byte(vm, Op.SUBTRACT);
		emit_bytes(vm, set_op, (Uint8)arg);
	} else if (can_assign && match(vm, STAR_EQUAL)) {
		emit_bytes(vm, get_op, (Uint8)arg);
		expression(vm);
		emit_byte(vm, Op.MULTIPLY);
		emit_bytes(vm, set_op, (Uint8)arg);
	} else if (can_assign && match(vm, SLASH_EQUAL)) {
		emit_bytes(vm, get_op, (Uint8)arg);
		expression(vm);
		emit_byte(vm, Op.DIVIDE);
		emit_bytes(vm, set_op, (Uint8)arg);
	} else {
		emit_bytes(vm, get_op, (Uint8)arg);
	}
}

fn void variable(Vm* vm, bool can_assign) {
	named_variable(vm, vm.compiler.parser.previous, can_assign);
}

fn void grouping(Vm* vm, bool can_assign) {
	expression(vm);
	consume(vm, RIGHT_PAREN, "Expect ')' after compiler_expression.");
}

fn void unary(Vm* vm, bool can_assign) {
	TokenType operator_type = vm.compiler.parser.previous.type;
	parse_precedence(vm, UNARY);

	switch (operator_type) {
		case BANG: emit_byte(vm, Op.NOT);
		case MINUS: emit_byte(vm, Op.NEGATE);
		default: return;
	}
}

fn void binary(Vm* vm, bool can_assign) {
	TokenType operator_type = vm.compiler.parser.previous.type;
	ParseRule* rule         = get_rule(operator_type);
	parse_precedence(vm, (Prec)(rule.precedence.ordinal + 1));

	// c3fmt off
	switch (operator_type) {
		case BANG_EQUAL:    emit_bytes(vm, Op.EQUAL, Op.NOT);
		case EQUAL_EQUAL:   emit_byte(vm, Op.EQUAL);
		case GREATER:       emit_byte(vm, Op.GREATER);
		case GREATER_EQUAL: emit_bytes(vm, Op.LESS, Op.NOT);
		case LESS:          emit_byte(vm, Op.LESS);
		case LESS_EQUAL:    emit_bytes(vm, Op.GREATER, Op.NOT);
		case PLUS:          emit_byte(vm, Op.ADD);
		case MINUS:         emit_byte(vm, Op.SUBTRACT);
		case STAR:          emit_byte(vm, Op.MULTIPLY);
		case SLASH:         emit_byte(vm, Op.DIVIDE);
		case PERCENT:       emit_byte(vm, Op.MODULO);
		default:            return;
	}
	// c3fmt on
}

fn void emit_constant(Vm* vm, Value value) {
	emit_bytes(vm, Op.CONSTANT, make_constant(vm, value));
}

fn void patch_jump(Vm* vm, sz offset) {
	sz jump = vm.chunk.count() - offset - 2;

	if (jump > UINT16_MAX) {
		error(vm, "Too much code to jump over.");
	}

	vm.chunk.code[offset] = (Uint8)((jump >> 8) & 0xff);
	vm.chunk.code[offset + 1] = (Uint8)(jump & 0xff);
}

fn void number(Vm* vm, bool can_assign) {
	double val = vm.compiler.parser.previous.lexeme.to_double()!!;
	emit_constant(vm, number_val(val));
}

fn void and_(Vm* vm, bool can_assign) {
	sz end_jump = emit_jump(vm, Op.JUMP_IF_FALSE);
	emit_byte(vm, Op.POP);
	parse_precedence(vm, AND);
	patch_jump(vm, end_jump);
}

fn void or_(Vm* vm, bool can_assign) {
	sz end_jump = emit_jump(vm, Op.JUMP_IF_TRUE);
	emit_byte(vm, Op.POP);
	parse_precedence(vm, OR);
	patch_jump(vm, end_jump);
}

fn void string(Vm* vm, bool can_assign) {
	String inner = vm.compiler.parser.previous.lexeme[1..^2];
	Obj* c       = vm.allocate_string(inner);
	emit_constant(vm, obj_val(c));
}

fn void literal(Vm* vm, bool can_assign) {
	switch (vm.compiler.parser.previous.type) {
		case FALSE: emit_byte(vm, Op.FALSE); break;
		case NIL: emit_byte(vm, Op.NIL); break;
		case TRUE: emit_byte(vm, Op.TRUE); break;
		default: return;  // Unreachable.
	}
}

fn void end(Vm* vm) {
	emit_return(vm);
	$if $feat(DEBUG_PRINT_CODE):
		if (!vm.compiler.parser.had_error) vm.chunk.disassemble_chunk("code");
	$endif
}

fn void begin_scope(Vm* vm) {
	vm.compiler.scope_depth++;
}

fn void end_scope(Vm* vm) {
	vm.compiler.scope_depth--;
	while (vm.compiler.locals_count > 0
		&& vm.compiler.locals[vm.compiler.locals_count - 1].depth > vm.compiler.scope_depth) {
		emit_byte(vm, Op.POP);
		vm.compiler.locals_count--;
	}
}

fn bool identifiers_equal(LoxToken* a, LoxToken* b) {
	if (a.lexeme.len != b.lexeme.len) return false;
	return (a.lexeme == b.lexeme);
}

fn sz resolve_local(Vm* vm, LoxToken* name) {
	for (sz i = vm.compiler.locals_count - 1; i >= 0; i--) {
		Local* local = &vm.compiler.locals[i];
		if (identifiers_equal(name, &local.name)) {
			if (local.depth == -1) {
				error(vm, "Can't read local variable in its own initializer.");
			}
		}
		return i;
	}
	return -1;
}

fn void add_local(Vm* vm, LoxToken name) {
	if (vm.compiler.locals_count == UINT8_COUNT) {
		error(vm, "Too man local variables in functions");
		return;
	}
	Local* local = &vm.compiler.locals[vm.compiler.locals_count++];
	local.name = name;
	local.depth = -1;
}

fn void mark_initialized(Vm* vm) {
	vm.compiler.locals[vm.compiler.locals_count - 1].depth = vm.compiler.scope_depth;
}

fn void define_variable(Vm* vm, Uint8 global) {
	if (vm.compiler.scope_depth > 0) {
		mark_initialized(vm);
		return;
	}
	emit_bytes(vm, Op.DEFINE_GLOBAL, global);
}

fn void expression_statement(Vm* vm) {
	expression(vm);
	consume(vm, SEMICOLON, "Expect ';' after expression.");
	emit_byte(vm, Op.POP);
}

fn void print_statement(Vm* vm) {
	expression(vm);
	consume(vm, SEMICOLON, "Expect ';' after value.");
	emit_byte(vm, Op.PRINT);
}

fn void if_statement(Vm* vm) {
	consume(vm, LEFT_PAREN, "Expect '(' after 'if',");
	expression(vm);
	consume(vm, RIGHT_PAREN, "Expect ')' after condition.");

	sz then_jump = emit_jump(vm, Op.JUMP_IF_FALSE);
	emit_byte(vm, Op.POP);
	statement(vm);

	sz else_jump = emit_jump(vm, Op.JUMP);
	patch_jump(vm, then_jump);
	emit_byte(vm, Op.POP);

	if (match(vm, ELSE)) statement(vm);
	patch_jump(vm, else_jump);
}

fn void continue_statement(Vm* vm) {
	if (vm.compiler.current_loop == null) {
		error(vm, "Can't use 'continue' outside of a loop.");
		return;
	}

	consume(vm, SEMICOLON, "Expect ';' after 'continue'.");

	Loop* loop = vm.compiler.current_loop;

	for (sz i = vm.compiler.locals_count; i > 0; i--) {
		Local* local = &vm.compiler.locals[i - 1];
		if (local.depth <= loop.scope_depth) break;
		emit_byte(vm, Op.POP);
	}

	emit_loop(vm, loop.continue_target);
}

fn void break_statement(Vm* vm) {
	if (vm.compiler.current_loop == null) {
		error(vm, "Can't use 'break' outside of a loop.");
		return;
	}

	consume(vm, SEMICOLON, "Expect ';' after 'break'.");

	Loop* loop = vm.compiler.current_loop;

	if (loop.break_count == MAX_BREAKS) {
		error(vm, "Too many break statements in loop.");
		return;
	}

	for (sz i = vm.compiler.locals_count; i > 0; i--) {
		Local* local = &vm.compiler.locals[i - 1];
		if (local.depth <= loop.scope_depth) break;
		emit_byte(vm, Op.POP);
	}

	sz jump = emit_jump(vm, Op.JUMP);

	loop.break_jumps[loop.break_count++] = jump;
}

fn void for_statement(Vm* vm) {
	begin_scope(vm);

	consume(vm, LEFT_PAREN, "Expect '(' after 'for'.");

	if (match(vm, SEMICOLON)) {
		// No initializer.
	} else if (match(vm, VAR)) {
		var_declaration(vm);
	} else {
		expression_statement(vm);
	}

	sz loop_start =  vm.chunk.count();
	sz exit_jump  = -1;

	if (!match(vm, SEMICOLON)) {
		expression(vm);
		consume(vm, SEMICOLON, "Expect ';' after loop condition.");

		exit_jump = emit_jump(vm, Op.JUMP_IF_FALSE);
		emit_byte(vm, Op.POP);
	}

	sz continue_target = loop_start;

	if (!match(vm, RIGHT_PAREN)) {
		sz body_jump = emit_jump(vm, Op.JUMP);

		sz increment_start = vm.chunk.count();
		continue_target = increment_start;

		expression(vm);
		emit_byte(vm, Op.POP);

		consume(vm, RIGHT_PAREN, "Expect ')' after for clauses.");

		emit_loop(vm, loop_start);

		loop_start = increment_start;

		patch_jump(vm, body_jump);
	}

	Loop loop = {
		.continue_target = continue_target,
		.break_count = 0,
		.scope_depth = vm.compiler.scope_depth,
		.enclosing = vm.compiler.current_loop,
	};

	vm.compiler.current_loop = &loop;

	statement(vm);

	emit_loop(vm, loop_start);

	if (exit_jump != -1) {
		patch_jump(vm, exit_jump);
		emit_byte(vm, Op.POP);
	}

	for (sz i = 0; i < loop.break_count; i++) {
		patch_jump(vm, loop.break_jumps[i]);
	}

	end_scope(vm);

	vm.compiler.current_loop = loop.enclosing;
}

fn void while_statement(Vm* vm) {
	sz loop_start = vm.chunk.count();

	Loop loop = {
		.continue_target = loop_start,
		.break_count = 0,
		.scope_depth = vm.compiler.scope_depth,
		.enclosing = vm.compiler.current_loop,
	};

	vm.compiler.current_loop = &loop;

	consume(vm, LEFT_PAREN, "Expect '(' after 'while'.");
	expression(vm);
	consume(vm, RIGHT_PAREN, "Expect ')' after condition.");

	sz exit_jump = emit_jump(vm, Op.JUMP_IF_FALSE);
	emit_byte(vm, Op.POP);

	statement(vm);

	emit_loop(vm, loop_start);

	patch_jump(vm, exit_jump);
	emit_byte(vm, Op.POP);

	for (sz i = 0; i < loop.break_count; i++) {
		patch_jump(vm, loop.break_jumps[i]);
	}
	
	vm.compiler.current_loop = loop.enclosing;
}

fn void statement(Vm* vm) {
	if (match(vm, PRINT)) {
		print_statement(vm);
	} else if (match(vm, BREAK)) {
		break_statement(vm);
	} else if (match(vm, CONTINUE)) {
		continue_statement(vm);
	} else if (match(vm, FOR)) {
		for_statement(vm);
	} else if (match(vm, IF)) {
		if_statement(vm);
	} else if (match(vm, WHILE)) {
		while_statement(vm);
	} else if (match(vm, LEFT_BRACE)) {
		begin_scope(vm);
		block(vm);
		end_scope(vm);
	} else {
		expression_statement(vm);
	}
}

fn void declare_variable(Vm* vm) {
	if (vm.compiler.scope_depth == 0) return;

	LoxToken* name = &vm.compiler.parser.previous;
	for (sz i = vm.compiler.locals_count - 1; i >= 0; i--) {
		Local* local = &vm.compiler.locals[i];
		if (local.depth != -1 && local.depth < vm.compiler.scope_depth) {
			break;
		}

		if (identifiers_equal(name, &local.name)) {
			error(vm, "Already a variable with this name in this scope.");
		}
	}
	add_local(vm, *name);
}

fn Uint8 parse_variable(Vm* vm, String error_message) {
	consume(vm, IDENTIFIER, error_message);

	declare_variable(vm);
	if (vm.compiler.scope_depth > 0) return 0;

	return identifier_constant(vm, &vm.compiler.parser.previous);
}

fn void var_declaration(Vm* vm) {
	Uint8 global = parse_variable(vm, "Expect variable name.");

	if (match(vm, EQUAL)) {
		expression(vm);
	} else {
		emit_byte(vm, Op.NIL);
	}
	consume(vm, SEMICOLON, "Expect ';' after variable declaration.");
	define_variable(vm, global);
}

fn void declaration(Vm* vm) {
	if (match(vm, VAR)) {
		var_declaration(vm);
	} else {
		statement(vm);
	}
	if (vm.compiler.parser.panic_mode) synchronize(vm);
}

fn void block(Vm* vm) {
	while (!check(vm, RIGHT_BRACE) && !check(vm, EOF)) {
		declaration(vm);
	}

	consume(vm, RIGHT_BRACE, "Expect '} after block");
}

fn void synchronize(Vm* vm) {
	vm.compiler.parser.panic_mode = false;

	while (vm.compiler.parser.current.type != EOF) {
		if (vm.compiler.parser.previous.type == SEMICOLON) return;
		switch (vm.compiler.parser.current.type) {
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

		advance(vm);
	}
}
fn bool compile(Vm* vm, char[] source, Chunk* new_chunk) {
	vm.compiler.init(source);
	vm.chunk = new_chunk;

	vm.compiler.parser.had_error = false;
	vm.compiler.parser.panic_mode = false;

	advance(vm);
	while (!match(vm, EOF)) {
		declaration(vm);
	}
	end(vm);
	return !vm.compiler.parser.had_error;
}
