module c3lox;

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

Parser parser;
Chunk* compiling_chunk;

fn Chunk* current_chunk() => compiling_chunk;

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

fn void compiler_advance() {
	parser.previous = parser.current;

	for (;;) {
		parser.current = scan_token();
		if (parser.current.type != ERROR) break;

		error_at_current(parser.current.lexeme);
	}
}

fn void compiler_consume(TokenType type, String message) {
	if (parser.current.type == type) {
		compiler_advance();
		return;
	}

	error_at_current(message);
}

fn void emit_byte(Uint8 byte) => current_chunk().write(byte, parser.previous.line);
fn void emit_return() => emit_byte(Op.RETURN.ordinal);

fn void end_compiler() {
	emit_return();
	$if $feat(DEBUG_PRINT_CODE):
		if (!parser.had_error) current_chunk().disassemble_chunk("code");
	$endif
}

fn void emit_bytes(Uint8 byte1, Uint8 byte2) {
	emit_byte(byte1);
	emit_byte(byte2);
}

fn void compiler_number() {
  double val = parser.previous.lexeme.to_double()!!;
  emit_constant({ (ValueData) { .as_num = val }, NUMBER });
}

fn ParseRule* get_rule(TokenType type) => type.rule;

fn void parse_precedence(Precedence precedence) {
	compiler_advance();
	ParseFn prefix_rule = get_rule(parser.previous.type).prefix;
	if (prefix_rule == null) {
		error("Expect expression.");
		return;
	}

	prefix_rule();

	while (precedence <= get_rule(parser.current.type).precedence) {
		compiler_advance();
		ParseFn infix_rule = get_rule(parser.previous.type).infix;
		infix_rule();
	}
}

fn void compiler_unary() {
	TokenType operator_type = parser.previous.type;
	parse_precedence(UNARY);

	switch (operator_type) {
		case MINUS: emit_byte(Op.NEGATE.ordinal);
		default: return;
	}
}

fn void compiler_binary() {
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

fn void compiler_expression() {
	parse_precedence(ASSIGNMENT);
}

fn void compiler_grouping() {
	compiler_expression();
	compiler_consume(RIGHT_PAREN, "Expect ')' after compiler_expression.");
}

fn Uint8 make_constant(Value value) {
	sz constant = current_chunk().add_constant(value);
	if (constant > UINT8_MAX) {
		error("Too many constants in one chunk.");
		return 0;
	}

	return (Uint8)constant;
}

fn void emit_constant(Value value) {
	emit_bytes(Op.CONSTANT.ordinal, make_constant(value));
}

fn bool compile(char[] source, Chunk* chunk) {
	scanner_init(source);
	compiling_chunk = chunk;

	parser.had_error = false;
	parser.panic_mode = false;

	compiler_advance();
	compiler_expression();
	compiler_consume(EOF, "Expect end of compiler_expression.");
	end_compiler();
	return !parser.had_error;
}