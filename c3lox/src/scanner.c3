module scanner;

import std::io;

enum TokenType {
	// Single-character tokens.
	LEFT_PAREN,
	RIGHT_PAREN,
	LEFT_BRACE,
	RIGHT_BRACE,
	COMMA,
	DOT,
	MINUS,
	PLUS,
	SEMICOLON,
	SLASH,
	STAR,

	// One or two character tokens.
	BANG,
	BANG_EQUAL,
	EQUAL,
	EQUAL_EQUAL,
	GREATER,
	GREATER_EQUAL,
	LESS,
	LESS_EQUAL,

	// Literals.
	IDENTIFIER,
	STRING,
	NUMBER,

	// Keywords.
	AND,
	CLASS,
	ELSE,
	FALSE,
	FOR,
	FUN,
	IF,
	NIL,
	OR,
	PRINT,
	RETURN,
	SUPER,
	THIS,
	TRUE,
	VAR,
	WHILE,

	ERROR,
	EOF
}

struct Token {
	TokenType type;
	String lexeme;
	sz line;
}

struct Scanner {
	char[] source;
	sz start;
	sz current;
	sz line;
}

Scanner scanner;

fn void init(char[] source) {
	scanner.source = source;
	scanner.start = 0;
	scanner.current = 0;
	scanner.line = 1;
}

fn bool is_alpha(char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

fn bool is_digit(char c) => c >= '0' && c <= '9';
fn bool at_end() => scanner.current >= scanner.source.len - 1;

fn char advance() {
	char c = scanner.source[scanner.current];
	scanner.current++;
	return c;
}

fn char peek() => scanner.source[scanner.current];

fn char peek_next() {
	if (at_end()) return '\0';
	return scanner.source[scanner.current + 1];
}

fn bool match(char expected) {
	if (at_end()) return false;
	if (scanner.source[scanner.current] != expected) return false;
	scanner.current++;
	return true;
}

fn Token make_token(TokenType type) {
	Token token;
	token.type = type;
	token.lexeme = (String)scanner.source[scanner.start..scanner.current - 1];
	token.line = scanner.line;
	return token;
}

fn Token error_token(String message) {
	Token token;
	token.type = ERROR;
	token.lexeme = message;
	token.line = scanner.line;
	return token;
}

fn void skip_whitespace() {
	for (;;) {
		char c = peek();
		switch (c) {
			case ' ':
			case '\r':
			case '\t':
				advance();
			case '\n':
				scanner.line++;
				advance();
			case '/':
				if (peek_next() == '/') {
					while (peek() != '\n' && !at_end()) advance();
				} else {
					return;
				}
			default:
				return;
		}
	}
}

fn TokenType check_keyword(sz start, sz length, ZString rest, TokenType type) {
	if (scanner.current - scanner.start == start + length
		&& scanner.source[scanner.start + start:length] == rest[:length]) {
		return type;
	}

	return IDENTIFIER;
}

fn TokenType identifier_type() {
	switch (scanner.source[scanner.start]) {
		case 'a': return check_keyword(1, 2, "nd", AND);
		case 'c': return check_keyword(1, 4, "lass", CLASS);
		case 'e': return check_keyword(1, 3, "lse", ELSE);
		case 'f':
			if (scanner.current - scanner.start > 1) {
				switch (scanner.source[scanner.start + 1]) {
					case 'a': return check_keyword(2, 3, "lse", FALSE);
					case 'o': return check_keyword(2, 1, "r", FOR);
					case 'u': return check_keyword(2, 1, "n", FUN);
				}
			}
		case 'i': return check_keyword(1, 1, "f", IF);
		case 'n': return check_keyword(1, 2, "il", NIL);
		case 'o': return check_keyword(1, 1, "r", OR);
		case 'p': return check_keyword(1, 4, "rint", PRINT);
		case 'r': return check_keyword(1, 5, "eturn", RETURN);
		case 's': return check_keyword(1, 4, "uper", SUPER);
		case 't':
			if (scanner.current - scanner.start > 1) {
				switch (scanner.source[scanner.start + 1]) {
					case 'h': return check_keyword(2, 2, "is", THIS);
					case 'r': return check_keyword(2, 2, "ue", TRUE);
				}
			}
		case 'v': return check_keyword(1, 2, "ar", VAR);
		case 'w': return check_keyword(1, 4, "hile", WHILE);
	}

	return IDENTIFIER;
}

fn Token identifier() {
	while (is_alpha(peek()) || is_digit(peek())) advance();
	return make_token(identifier_type());
}

fn Token number() {
	while (is_digit(peek())) advance();

	if (peek() == '.' && is_digit(peek_next())) {
		advance();
		while (is_digit(peek())) advance();
	}

	return make_token(NUMBER);
}

fn Token string() {
	while (peek() != '"') {
		if (peek() == '\n') {
			scanner.line++;
			advance();
		}
	}

	if (at_end()) return error_token("Unterminated string.");
	advance();
	return make_token(STRING);
}

fn Token scan_token() {
	skip_whitespace();
	scanner.start = scanner.current;

	if (at_end()) {
		return make_token(EOF);
	}

	char c = advance();
	if (is_alpha(c)) return identifier();
	if (is_digit(c)) return number();

	switch (c) {
		case '(': return make_token(LEFT_PAREN);
		case ')': return make_token(RIGHT_PAREN);
		case '{': return make_token(LEFT_BRACE);
		case '}': return make_token(RIGHT_BRACE);
		case ';': return make_token(SEMICOLON);
		case ',': return make_token(COMMA);
		case '.': return make_token(DOT);
		case '-': return make_token(MINUS);
		case '+': return make_token(PLUS);
		case '/': return make_token(SLASH);
		case '*': return make_token(STAR);
		case '!': return make_token(match('=') ? BANG_EQUAL : BANG);
		case '=': return make_token(match('=') ? EQUAL_EQUAL : EQUAL);
		case '<': return make_token(match('=') ? LESS_EQUAL : LESS);
		case '>': return make_token(match('=') ? GREATER_EQUAL : GREATER);
		case '"': return string();
	}

	return error_token("Unexpected character.");
}