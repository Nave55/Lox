module c3lox;

import std::io;

struct Parser {
	LoxToken current;
	LoxToken previous;
	bool had_error;
	bool panic_mode;
}

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

struct Local {
	LoxToken name;
	sz depth;
}

struct Compiler {
	Local[UINT8_COUNT] locals;
	sz local_count;
	sz scope_depth;

	LoxScanner scanner;
	Parser parser;
	Chunk* chunk;
}

// c3fmt off
const ParseRule*[*] RULE = {
	[TokenType.LEFT_PAREN]    = &&{ &Compiler.grouping, null,    Precedence.NONE },
	[TokenType.RIGHT_PAREN]   = &&{ null,      null,    Precedence.NONE },
	[TokenType.LEFT_BRACE]    = &&{ null,      null,    Precedence.NONE },
	[TokenType.RIGHT_BRACE]   = &&{ null,      null,    Precedence.NONE },
	[TokenType.COMMA]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.DOT]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.MINUS]         = &&{ &Compiler.unary,    &Compiler.binary, Precedence.TERM },
	[TokenType.PLUS]          = &&{ null,      &Compiler.binary, Precedence.TERM },
	[TokenType.SEMICOLON]     = &&{ null,      null,    Precedence.NONE },
	[TokenType.SLASH]         = &&{ null,      &Compiler.binary, Precedence.FACTOR },
	[TokenType.STAR]          = &&{ null,      &Compiler.binary, Precedence.FACTOR },
	[TokenType.BANG]          = &&{ &Compiler.unary,    null,    Precedence.NONE },
	[TokenType.BANG_EQUAL]    = &&{ null,      &Compiler.binary, Precedence.EQUALITY },
	[TokenType.EQUAL]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.EQUAL_EQUAL]   = &&{ null,      &Compiler.binary, Precedence.EQUALITY },
	[TokenType.GREATER]       = &&{ null,      &Compiler.binary, Precedence.COMPARISON },
	[TokenType.GREATER_EQUAL] = &&{ null,      &Compiler.binary, Precedence.COMPARISON },
	[TokenType.LESS]          = &&{ null,      &Compiler.binary, Precedence.COMPARISON },
	[TokenType.LESS_EQUAL]    = &&{ null,      &Compiler.binary, Precedence.COMPARISON },
	[TokenType.IDENTIFIER]    = &&{ &Compiler.variable, null,    Precedence.NONE },
	[TokenType.STRING]        = &&{ &Compiler.string,   null,    Precedence.NONE },
	[TokenType.NUMBER]        = &&{ &Compiler.number,   null,    Precedence.NONE },
	[TokenType.AND]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.CLASS]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.ELSE]          = &&{ null,      null,    Precedence.NONE },
	[TokenType.FALSE]         = &&{ &Compiler.literal,  null,    Precedence.NONE },
	[TokenType.FOR]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.FUN]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.IF]            = &&{ null,      null,    Precedence.NONE },
	[TokenType.NIL]           = &&{ &Compiler.literal,  null,    Precedence.NONE },
	[TokenType.OR]            = &&{ null,      null,    Precedence.NONE },
	[TokenType.PRINT]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.RETURN]        = &&{ null,      null,    Precedence.NONE },
	[TokenType.SUPER]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.THIS]          = &&{ null,      null,    Precedence.NONE },
	[TokenType.TRUE]          = &&{ &Compiler.literal,  null,    Precedence.NONE },
	[TokenType.VAR]           = &&{ null,      null,    Precedence.NONE },
	[TokenType.WHILE]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.ERROR]         = &&{ null,      null,    Precedence.NONE },
	[TokenType.EOF]           = &&{ null,      null,    Precedence.NONE },
};
// c3fmt on

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
}

fn void Compiler.number(Compiler* compiler, bool can_assign) {
	double val = compiler.parser.previous.lexeme.to_double()!!;
	compiler.emit_constant(number_val(val));
}

fn void Compiler.string(Compiler* compiler, bool can_assign) {
	sz len       = compiler.parser.previous.lexeme.len;
	String inner = compiler.parser.previous.lexeme[1..^2];
	Obj* c       = allocate_string(inner);
	compiler.emit_constant(obj_val(c));
}

fn void Compiler.named_variable(Compiler* compiler, LoxToken name, bool can_assign) {
	Uint8 arg = compiler.identifier_constant(&name);
	if (can_assign && compiler.match(EQUAL)) {
		compiler.expression();
		compiler.emit_bytes(Op.SET_GLOBAL, arg);
	} else {
		compiler.emit_bytes(Op.GET_GLOBAL, arg);
	}
}

fn void Compiler.variable(Compiler* compiler, bool can_assign) {
	compiler.named_variable(compiler.parser.previous, can_assign);
}

fn void Compiler.parse_precedence(Compiler* compiler, Precedence precedence) {
	compiler.advance();
	ParseFn prefix_rule = compiler.get_rule(compiler.parser.previous.type).prefix;
	if (prefix_rule == null) {
		compiler.error("Expect expression.");
		return;
	}

	bool can_assign = precedence <= ASSIGNMENT;
	prefix_rule(compiler, can_assign);

	while (precedence <= compiler.get_rule(compiler.parser.current.type).precedence) {
		compiler.advance();
		ParseFn infix_rule = compiler.get_rule(compiler.parser.previous.type).infix;
		infix_rule(compiler, can_assign);
	}

	if (can_assign && compiler.match(EQUAL)) {
		compiler.error("Invalid assignment target.");
	}
}

fn Uint8 Compiler.identifier_constant(Compiler* compiler, LoxToken* name) {
	Uint8? val = vm.chunk.idents.get(name.lexeme);
	if (try val) return val;

	Uint8 idx = compiler.make_constant(obj_val(allocate_ident(name.lexeme)));
	vm.chunk.idents.set(name.lexeme, idx);
	return idx;
}

fn Uint8 Compiler.parse_variable(Compiler* compiler, String error_message) {
	compiler.consume(IDENTIFIER, error_message);
	return compiler.identifier_constant(&compiler.parser.previous);
}

fn void Compiler.define_variable(Compiler* compiler, Uint8 global) {
	compiler.emit_bytes(Op.DEFINE_GLOBAL, global);
}

fn void Compiler.unary(Compiler* compiler, bool can_assign) {
	TokenType operator_type = compiler.parser.previous.type;
	compiler.parse_precedence(UNARY);

	switch (operator_type) {
		case BANG: compiler.emit_byte(Op.NOT);
		case MINUS: compiler.emit_byte(Op.NEGATE);
		default: return;
	}
}

fn void Compiler.binary(Compiler* compiler, bool can_assign) {
	TokenType operator_type = compiler.parser.previous.type;
	ParseRule* rule         = compiler.get_rule(operator_type);
	compiler.parse_precedence((Precedence)(rule.precedence.ordinal + 1));

	// c3fmt off
	switch (operator_type) {
		case BANG_EQUAL:    compiler.emit_bytes(Op.EQUAL, Op.NOT);
		case EQUAL_EQUAL:   compiler.emit_byte(Op.EQUAL);
		case GREATER:       compiler.emit_byte(Op.GREATER);
		case GREATER_EQUAL: compiler.emit_bytes(Op.LESS, Op.NOT);
		case LESS:          compiler.emit_byte(Op.LESS);
		case LESS_EQUAL:    compiler.emit_bytes(Op.GREATER, Op.NOT);
		case PLUS:          compiler.emit_byte(Op.ADD);
		case MINUS:         compiler.emit_byte(Op.SUBTRACT);
		case STAR:          compiler.emit_byte(Op.MULTIPLY);
		case SLASH:         compiler.emit_byte(Op.DIVIDE);
		default:            return;
	}
	// c3fmt on
}

fn void Compiler.literal(Compiler* compiler, bool can_assign) {
	switch (compiler.parser.previous.type) {
		case FALSE: compiler.emit_byte(Op.FALSE); break;
		case NIL: compiler.emit_byte(Op.NIL); break;
		case TRUE: compiler.emit_byte(Op.TRUE); break;
		default: return;  // Unreachable.
	}
}

fn void Compiler.expression(Compiler* compiler) {
	compiler.parse_precedence(ASSIGNMENT);
}

fn void Compiler.block(Compiler* compiler) {
	while (!compiler.check(RIGHT_BRACE) && !compiler.check(EOF)) {
		compiler.declaration();
	}

	compiler.consume(RIGHT_BRACE, "Expect '} after block");
}

fn void Compiler.expression_statement(Compiler* compiler) {
	compiler.expression();
	compiler.consume(SEMICOLON, "Expect ';' after expression.");
	compiler.emit_byte(Op.POP);
}

fn void Compiler.var_declaration(Compiler* compiler) {
	Uint8 global = compiler.parse_variable("Expect variable name.");

	if (compiler.match(EQUAL)) {
		compiler.expression();
	} else {
		compiler.emit_byte(Op.NIL);
	}
	compiler.consume(SEMICOLON, "Expect ';' after variable declaration.");
	compiler.define_variable(global);
}

fn void Compiler.print_statement(Compiler* compiler) {
	compiler.expression();
	compiler.consume(SEMICOLON, "Expect ';' after value.");
	compiler.emit_byte(Op.PRINT);
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
fn void Compiler.declaration(Compiler* compiler) {
	if (compiler.match(VAR)) {
		compiler.var_declaration();
	} else {
		compiler.statement();
	}
	if (compiler.parser.panic_mode) compiler.synchronize();
}

fn void Compiler.statement(Compiler* compiler) {
	if (compiler.match(PRINT)) {
		compiler.print_statement();
	} else if (compiler.match(LEFT_BRACE)) {
		compiler.begin_scope();
		compiler.block();
		compiler.end_scope();
	} else {
		compiler.expression_statement();
	}
}

fn void Compiler.grouping(Compiler* compiler, bool can_assign) {
	compiler.expression();
	compiler.consume(RIGHT_PAREN, "Expect ')' after compiler_expression.");
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

fn bool compile(char[] source, Chunk* new_chunk) {
	Compiler compiler;
	compiler.init(source);
	compiler.chunk = new_chunk;

	compiler.parser.had_error = false;
	compiler.parser.panic_mode = false;

	compiler.advance();
	while (!compiler.match(EOF)) {
		compiler.declaration();
	}
	compiler.end();
	return !compiler.parser.had_error;
}

fn void Compiler.init(Compiler* compiler, char[] source) {
	compiler.scanner.init(source);
	compiler.local_count = 0;
	compiler.scope_depth = 0;
}
