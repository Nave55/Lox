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

enum Rule : Uint8 (ParseRule* rule) {
	LEFT_PAREN    { &&{ &grouping, null, Precedence.NONE } },
	RIGHT_PAREN   { &&{ null, null, Precedence.NONE } },
	LEFT_BRACE    { &&{ null, null, Precedence.NONE } },
	RIGHT_BRACE   { &&{ null, null, Precedence.NONE } },
	COMMA         { &&{ null, null, Precedence.NONE } },
	DOT           { &&{ null, null, Precedence.NONE } },
	MINUS         { &&{ &unary, &binary, Precedence.TERM } },
	PLUS          { &&{ null, &binary, Precedence.TERM } },
	SEMICOLON     { &&{ null, null, Precedence.NONE } },
	SLASH         { &&{ null, &binary, Precedence.FACTOR } },
	STAR          { &&{ null, &binary, Precedence.FACTOR } },
	BANG          { &&{ null, null, Precedence.NONE } },
	BANG_EQUAL    { &&{ null, null, Precedence.NONE } },
	EQUAL         { &&{ null, null, Precedence.NONE } },
	EQUAL_EQUAL   { &&{ null, null, Precedence.NONE } },
	GREATER       { &&{ null, null, Precedence.NONE } },
	GREATER_EQUAL { &&{ null, null, Precedence.NONE } },
	LESS          { &&{ null, null, Precedence.NONE } },
	LESS_EQUAL    { &&{ null, null, Precedence.NONE } },
	IDENTIFIER    { &&{ null, null, Precedence.NONE } },
	STRING        { &&{ null, null, Precedence.NONE } },
	NUMBER        { &&{ &number, null, Precedence.NONE } },
	AND           { &&{ null, null, Precedence.NONE } },
	CLASS         { &&{ null, null, Precedence.NONE } },
	ELSE          { &&{ null, null, Precedence.NONE } },
	FALSE         { &&{ null, null, Precedence.NONE } },
	FOR           { &&{ null, null, Precedence.NONE } },
	FUN           { &&{ null, null, Precedence.NONE } },
	IF            { &&{ null, null, Precedence.NONE } },
	NIL           { &&{ null, null, Precedence.NONE } },
	OR            { &&{ null, null, Precedence.NONE } },
	PRINT         { &&{ null, null, Precedence.NONE } },
	RETURN        { &&{ null, null, Precedence.NONE } },
	SUPER         { &&{ null, null, Precedence.NONE } },
	THIS          { &&{ null, null, Precedence.NONE } },
	TRUE          { &&{ null, null, Precedence.NONE } },
	VAR           { &&{ null, null, Precedence.NONE } },
	WHILE         { &&{ null, null, Precedence.NONE } },
	ERROR         { &&{ null, null, Precedence.NONE } },
	EOF           { &&{ null, null, Precedence.NONE } },
}

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

	io::eprintf(": %s\n", message);
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
fn void emit_return() => emit_byte(Op.RETURN.ordinal);
fn ParseRule* get_rule(TokenType type) => ((Rule)type.ordinal).rule;

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
	emit_constant(@to_value(val));
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
		case MINUS: emit_byte(Op.NEGATE.ordinal);
		default: return;
	}
}

fn void binary() {
	TokenType operator_type = parser.previous.type;
	ParseRule* rule         = get_rule(operator_type);
	parse_precedence((Precedence)(rule.precedence.ordinal + 1));

	switch (operator_type) {
		case PLUS: emit_byte(Op.ADD.ordinal);
		case MINUS: emit_byte(Op.SUBTRACT.ordinal);
		case STAR: emit_byte(Op.MULTIPLY.ordinal);
		case SLASH: emit_byte(Op.DIVIDE.ordinal);
		default: return;
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
	emit_bytes(Op.CONSTANT.ordinal, make_constant(value));
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