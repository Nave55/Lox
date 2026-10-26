module c3lox;

import std::io;

// c3fmt off
enum TokenType : char {
	// Single-character tokens.
	LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
	COMMA,      DOT,         MINUS,      PLUS,
	SEMICOLON,  SLASH,       STAR,       PERCENT,

	// One or two character tokens
	BANG,       BANG_EQUAL,    EQUAL,      EQUAL_EQUAL,
	GREATER,    GREATER_EQUAL, LESS,       LESS_EQUAL,
	PLUS_EQUAL, MINUS_EQUAL,   STAR_EQUAL, SLASH_EQUAL,

	// Literals
	IDENTIFIER, STRING, NUMBER,

	// Keywords
	AND,  CLASS, ELSE,   FALSE,
	FOR,  FUN,   IF,     NIL,
	OR,   PRINT, RETURN, SUPER,
	THIS, TRUE,  VAR,    WHILE,

	ERROR, EOF,
}
// c3fmt on

struct LoxToken {
	TokenType type;
	String lexeme;
	sz line;
}

struct LoxScanner {
	char[] source;
	sz start;
	sz current;
	sz line;
}

fn LoxToken LoxScanner.make_token(LoxScanner* scanner, TokenType type) {
	LoxToken token;
	token.type = type;
	token.lexeme = (String)scanner.source[scanner.start..scanner.current - 1];
	token.line = scanner.line;
	return token;
}

fn LoxToken LoxScanner.error_token(LoxScanner* scanner, String message) {
	LoxToken token;
	token.type = ERROR;
	token.lexeme = message;
	token.line = scanner.line;
	return token;
}

fn void LoxScanner.init(LoxScanner* scanner, char[] source) {
	scanner.source = source;
	scanner.start = 0;
	scanner.current = 0;
	scanner.line = 1;
}

fn bool is_alpha(char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

fn bool is_digit(char c) => c >= '0' && c <= '9';

fn bool LoxScanner.is_at_end(LoxScanner* scanner, sz off = 0) {
	return scanner.current + off >= scanner.source.len;
}

fn char LoxScanner.advance(LoxScanner* scanner) {
	char c = scanner.source[scanner.current];
	scanner.current++;
	return c;
}

fn char LoxScanner.peek(LoxScanner* scanner) {
	if (scanner.is_at_end()) return '\0';
	return scanner.source[scanner.current];
}

fn char LoxScanner.peek_next(LoxScanner* scanner) {
	if (scanner.is_at_end(1)) return '\0';
	return scanner.source[scanner.current + 1];
}

fn bool LoxScanner.match(LoxScanner* scanner, char expected) {
	if (scanner.is_at_end()) return false;
	if (scanner.source[scanner.current] != expected) return false;
	scanner.current++;
	return true;
}

fn void LoxScanner.skip_whitespace(LoxScanner* scanner) {
	for (;;) {
		char c = scanner.peek();

		switch (c) {
			case ' ':
			case '\r':
			case '\t':
				scanner.advance();

			case '\n':
				scanner.line++;
				scanner.advance();

			case '/':
				if (scanner.peek_next() == '/') {
					while (scanner.peek() != '\n' && !scanner.is_at_end()) {
						scanner.advance();
					}
				} else {
					return;
				}

			default: return;
		}
	}
}

fn TokenType LoxScanner.check_keyword(
	LoxScanner* scanner,
	sz start,
	sz length,
	String rest,
	TokenType type
) {
	if (scanner.current - scanner.start == start + length
		&& scanner.source[scanner.start + start:length] == rest) {
		return type;
	}

	return IDENTIFIER;
}

fn TokenType LoxScanner.identifier_type(LoxScanner* scanner) {
	switch (scanner.source[scanner.start]) {
		case 'a': return scanner.check_keyword(1, 2, "nd", AND);
		case 'c': return scanner.check_keyword(1, 4, "lass", CLASS);
		case 'e': return scanner.check_keyword(1, 3, "lse", ELSE);

		case 'f':
			if (scanner.current - scanner.start > 1) {
				switch (scanner.source[scanner.start + 1]) {
					case 'a': return scanner.check_keyword(2, 3, "lse", FALSE);
					case 'o': return scanner.check_keyword(2, 1, "r", FOR);
					case 'u': return scanner.check_keyword(2, 1, "n", FUN);
				}
			}

		case 'i': return scanner.check_keyword(1, 1, "f", IF);
		case 'n': return scanner.check_keyword(1, 2, "il", NIL);
		case 'o': return scanner.check_keyword(1, 1, "r", OR);
		case 'p': return scanner.check_keyword(1, 4, "rint", PRINT);
		case 'r': return scanner.check_keyword(1, 5, "eturn", RETURN);
		case 's': return scanner.check_keyword(1, 4, "uper", SUPER);

		case 't':
			if (scanner.current - scanner.start > 1) {
				switch (scanner.source[scanner.start + 1]) {
					case 'h': return scanner.check_keyword(2, 2, "is", THIS);
					case 'r': return scanner.check_keyword(2, 2, "ue", TRUE);
				}
			}

		case 'v': return scanner.check_keyword(1, 2, "ar", VAR);
		case 'w': return scanner.check_keyword(1, 4, "hile", WHILE);
	}

	return IDENTIFIER;
}

fn LoxToken LoxScanner.identifier(LoxScanner* scanner) {
	while (is_alpha(scanner.peek()) || is_digit(scanner.peek())) {
		scanner.advance();
	}

	return scanner.make_token(scanner.identifier_type());
}

fn LoxToken LoxScanner.number(LoxScanner* scanner) {
	while (is_digit(scanner.peek())) {
		scanner.advance();
	}

	if (scanner.peek() == '.' && is_digit(scanner.peek_next())) {
		scanner.advance();
		while (is_digit(scanner.peek())) scanner.advance();
	}

	return scanner.make_token(NUMBER);
}

fn LoxToken LoxScanner.string(LoxScanner* scanner) {
	for (;;) {
		char val = scanner.peek();
		if (val == '"' || val == '\0') break;
		if (val == '\n') scanner.line++;
		scanner.advance();
	}

	if (scanner.is_at_end()) {
		return scanner.error_token("Unterminated string.");
	}

	scanner.advance();
	return scanner.make_token(STRING);
}

fn LoxToken LoxScanner.scan_token(LoxScanner* scanner) {
	scanner.skip_whitespace();
	scanner.start = scanner.current;

	if (scanner.is_at_end()) return scanner.make_token(EOF);
	char c = scanner.advance();

	if (is_alpha(c)) return scanner.identifier();
	if (is_digit(c)) return scanner.number();

	switch (c) {
		case '(': return scanner.make_token(LEFT_PAREN);
		case ')': return scanner.make_token(RIGHT_PAREN);
		case '{': return scanner.make_token(LEFT_BRACE);
		case '}': return scanner.make_token(RIGHT_BRACE);
		case ';': return scanner.make_token(SEMICOLON);
		case ',': return scanner.make_token(COMMA);
		case '.': return scanner.make_token(DOT);
		case '%': return scanner.make_token(PERCENT);
		case '-': return scanner.make_token(scanner.match('=') ? MINUS_EQUAL : MINUS);
		case '+': return scanner.make_token(scanner.match('=') ? PLUS_EQUAL : PLUS);
		case '*': return scanner.make_token(scanner.match('=') ? STAR_EQUAL : STAR);
		case '/': return scanner.make_token(scanner.match('=') ? SLASH_EQUAL : SLASH);
		case '!': return scanner.make_token(scanner.match('=') ? BANG_EQUAL : BANG);
		case '=': return scanner.make_token(scanner.match('=') ? EQUAL_EQUAL : EQUAL);
		case '<': return scanner.make_token(scanner.match('=') ? LESS_EQUAL : LESS);
		case '>': return scanner.make_token(scanner.match('=') ? GREATER_EQUAL : GREATER);
		case '"': return scanner.string();
	}

	return scanner.error_token(string::tformat("Unexpected character '%c'.", c));
}
