module c3lox;

import scanner;
import std::io;

const UINT8_MAX = 255;

struct Parser {
	Token current;
	Token previous;
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
	[TokenType.IDENTIFIER]    = &&{ null,      null,    Precedence.NONE },
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

Parser parser;
Chunk* chunk @local = null;

fn void error_at(Token* token, String message) {
	if (parser.panic_mode) return;
	parser.panic_mode = true;
	io::eprintf("[line %d] Error", token.line);

	if (token.type == EOF) {
		io::eprintf(" at end");
	} else if (token.type == ERROR) {
		// Nothing.
	} else {
		io::eprintf(" at '%s'", token.lexeme);
	}

	io::eprintfn(": %s", message);
	parser.had_error = true;
}

fn void error(String message) => error_at(&parser.previous, message);
fn void error_at_current(String message) => error_at(&parser.current, message);

fn void advance() {
	parser.previous = parser.current;

	for (;;) {
		parser.current = scanner::scan_token();
		if (parser.current.type != ERROR) break;

		error_at_current(parser.current.lexeme);
	}
}

fn void consume(TokenType type, String message) {
	if (parser.current.type == type) {
		advance();
		return;
	}

	error_at_current(message);
}

fn void emit_byte(Uint8 byte) => chunk.write(byte, parser.previous.line);
fn void emit_return() => emit_byte(Op.RETURN);
fn ParseRule* get_rule(TokenType type) => RULE[type];

fn void emit_bytes(Uint8 byte1, Uint8 byte2) {
	emit_byte(byte1);
	emit_byte(byte2);
}

fn void end_compiler() {
	emit_return();
	$if $feat(DEBUG_PRINT_CODE):
		if (!parser.had_error) chunk.disassemble_chunk("code");
	$endif
}

fn void number() {
	double val = parser.previous.lexeme.to_double()!!;
	emit_constant(number_val(val));
}

fn void string() {
	sz len       = parser.previous.lexeme.len;
	String inner = parser.previous.lexeme[1..^2];
	Obj* c       = allocate_string(inner);
	emit_constant(obj_val(c));
}

fn void parse_precedence(Precedence precedence) {
	advance();
	ParseFn prefix_rule = get_rule(parser.previous.type).prefix;
	if (prefix_rule == null) {
		error("Expect expression.");
		return;
	}

	prefix_rule();

	while (precedence <= get_rule(parser.current.type).precedence) {
		advance();
		ParseFn infix_rule = get_rule(parser.previous.type).infix;
		infix_rule();
	}
}

fn void unary() {
	TokenType operator_type = parser.previous.type;
	parse_precedence(UNARY);

	switch (operator_type) {
		case BANG: emit_byte(Op.NOT);
		case MINUS: emit_byte(Op.NEGATE);
		default: return;
	}
}

fn void binary() {
	TokenType operator_type = parser.previous.type;
	ParseRule* rule         = get_rule(operator_type);
	parse_precedence((Precedence)(rule.precedence.ordinal + 1));

	switch (operator_type) {
		case BANG_EQUAL: emit_bytes(Op.EQUAL, Op.NOT);
		case EQUAL_EQUAL: emit_byte(Op.EQUAL);
		case GREATER: emit_byte(Op.GREATER);
		case GREATER_EQUAL: emit_bytes(Op.LESS, Op.NOT);
		case LESS: emit_byte(Op.LESS);
		case LESS_EQUAL: emit_bytes(Op.GREATER, Op.NOT);
		case PLUS: emit_byte(Op.ADD);
		case MINUS: emit_byte(Op.SUBTRACT);
		case STAR: emit_byte(Op.MULTIPLY);
		case SLASH: emit_byte(Op.DIVIDE);
		default: return;
	}
}

fn void literal() {
	switch (parser.previous.type) {
		case FALSE: emit_byte(Op.FALSE); break;
		case NIL: emit_byte(Op.NIL); break;
		case TRUE: emit_byte(Op.TRUE); break;
		default: return;  // Unreachable.
	}
}

fn void expression() {
	parse_precedence(ASSIGNMENT);
}

fn void grouping() {
	expression();
	consume(RIGHT_PAREN, "Expect ')' after compiler_expression.");
}

fn Uint8 make_constant(Value value) {
	sz constant = chunk.add_constant(value);
	if (constant > UINT8_MAX) {
		error("Too many constants in one chunk.");
		return 0;
	}

	return (Uint8)constant;
}

fn void emit_constant(Value value) {
	emit_bytes(Op.CONSTANT, make_constant(value));
}

fn bool compile(char[] source, Chunk* new_chunk) {
	scanner::init(source);
	chunk = new_chunk;

	parser.had_error = false;
	parser.panic_mode = false;

	advance();
	expression();
	consume(EOF, "Expect end of compiler_expression.");
	end_compiler();
	return !parser.had_error;
}
