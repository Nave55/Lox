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

fn bool check(TokenType type) => parser.current.type == type;

fn bool match(TokenType type) {
	if (!check(type)) return false;
	advance();
	return true;
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

fn void number(bool can_assign) {
	double val = parser.previous.lexeme.to_double()!!;
	emit_constant(number_val(val));
}

fn void string(bool can_assign) {
	sz len       = parser.previous.lexeme.len;
	String inner = parser.previous.lexeme[1..^2];
	Obj* c       = allocate_string(inner);
	emit_constant(obj_val(c));
}

fn void named_variable(Token name, bool can_assign) {
	Uint8 arg = identifier_constant(&name);
    if (can_assign && match(EQUAL)) {
        expression();
        emit_bytes(Op.SET_GLOBAL, arg);
      } else {
        emit_bytes(Op.GET_GLOBAL, arg);
    }
  }

fn void variable(bool can_assign) => named_variable(parser.previous, can_assign);

fn void parse_precedence(Precedence precedence) {
	advance();
	ParseFn prefix_rule = get_rule(parser.previous.type).prefix;
	if (prefix_rule == null) {
		error("Expect expression.");
		return;
	}

	bool can_assign = precedence <= ASSIGNMENT;
	prefix_rule(can_assign);
	
	while (precedence <= get_rule(parser.current.type).precedence) {
		advance();
		ParseFn infix_rule = get_rule(parser.previous.type).infix;
		infix_rule(can_assign);
	}

  if (can_assign && match(EQUAL)) {
    error("Invalid assignment target.");
  }
}

fn Uint8 identifier_constant(Token* name) {
	return make_constant(obj_val(allocate_string(name.lexeme)));
}

fn Uint8 parse_variable(String error_message) {
	consume(IDENTIFIER, error_message);
	return identifier_constant(&parser.previous);
}

fn void define_variable(Uint8 global) {
	emit_bytes(Op.DEFINE_GLOBAL, global);
}

fn void unary(bool can_assign) {
	TokenType operator_type = parser.previous.type;
	parse_precedence(UNARY);

	switch (operator_type) {
		case BANG: emit_byte(Op.NOT);
		case MINUS: emit_byte(Op.NEGATE);
		default: return;
	}
}

fn void binary(bool can_assign) {
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

fn void literal(bool can_assign) {
	switch (parser.previous.type) {
		case FALSE: emit_byte(Op.FALSE); break;
		case NIL: emit_byte(Op.NIL); break;
		case TRUE: emit_byte(Op.TRUE); break;
		default: return;  // Unreachable.
	}
}

fn void expression() => parse_precedence(ASSIGNMENT);

fn void expression_statement() {
	expression();
	consume(SEMICOLON, "Expect ';' after expression.");
	emit_byte(Op.POP);
}

fn void var_declaration() {
	Uint8 global = parse_variable("Expect variable name.");

	if (match(EQUAL)) {
		expression();
	} else {
		emit_byte(Op.NIL);
	}
	consume(SEMICOLON, "Expect ';' after variable declaration.");
	define_variable(global);
}

fn void print_statement() {
	expression();
	consume(SEMICOLON, "Expect ';' after value.");
	emit_byte(Op.PRINT);
}

fn void synchronize() {
	parser.panic_mode = false;

	while (parser.current.type != EOF) {
		if (parser.previous.type == SEMICOLON) return;
		switch (parser.current.type) {
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

		advance();
	}
}
fn void declaration() {
	if (match(VAR)) {
		var_declaration();
	} else {
		statement();
	}
	if (parser.panic_mode) synchronize();
}

fn void statement() {
	if (match(PRINT)) {
		print_statement();
	} else {
		expression_statement();
	}
}

fn void grouping(bool can_assign) {
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
	while (!match(EOF)) {
		declaration();
	}
	end_compiler();
	return !parser.had_error;
}
