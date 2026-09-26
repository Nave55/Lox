module c3lox;

import std::io;

struct Scanner {
	char[] source;
	sz start;
	sz current;
	sz line;
}

Scanner scanner;

fn void scanner_init(char[] source) {
	scanner.source = source;
	scanner.start = 0;
	scanner.current = 0;
	scanner.line = 1;
}

fn bool is_alpha(char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

fn bool is_digit(char c) => c >= '0' && c <= '9';
fn bool scanner_is_at_end(sz off = 0) => scanner.current + off >= scanner.source.len;

fn char scanner_advance() {
	char c = scanner.source[scanner.current];
	scanner.current++;
	return c;
}

fn char scanner_peek() {
	if (scanner_is_at_end()) return '\0';
	return scanner.source[scanner.current];
}

fn char scanner_peek_next() {
	if (scanner_is_at_end(1)) return '\0';
	return scanner.source[scanner.current + 1];
}

fn bool scanner_match(char expected) {
	if (scanner_is_at_end()) return false;
	if (scanner.source[scanner.current] != expected) return false;
	scanner.current++;
	return true;
}

fn void scanner_skip_whitespace() {
	for (;;) {
		char c = scanner_peek();
		switch (c) {
			case ' ':
			case '\r':
			case '\t':
				scanner_advance();
			case '\n':
				scanner.line++;
				scanner_advance();
			case '/':
				if (scanner_peek_next() == '/') {
					while (scanner_peek() != '\n' && !scanner_is_at_end()) scanner_advance();
				} else {
					return;
				}
			default:
				return;
		}
	}
}

fn TokenType check_keyword(sz start, sz length, String rest, TokenType type) {
	if (scanner.current - scanner.start == start + length
		&& scanner.source[scanner.start + start:length] == rest) {
		return type;
	}

	return IDENTIFIER;
}

fn TokenType scanner_identifier_type() {
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

fn Token scanner_identifier() {
	while (is_alpha(scanner_peek()) || is_digit(scanner_peek())) scanner_advance();
	return make_token(scanner_identifier_type());
}

fn Token scanner_number() {
	while (is_digit(scanner_peek())) scanner_advance();

	if (scanner_peek() == '.' && is_digit(scanner_peek_next())) {
		scanner_advance();
		while (is_digit(scanner_peek())) scanner_advance();
	}

	return make_token(NUMBER);
}

fn Token scanner_string() {
	while (scanner_peek() != '"') {
		if (scanner_peek() == '\n') {
			scanner.line++;
			scanner_advance();
		}
	}

	if (scanner_is_at_end()) return error_token("Unterminated string.");
	scanner_advance();
	return make_token(STRING);
}

fn Token scan_token() {
	scanner_skip_whitespace();
	scanner.start = scanner.current;

	if (scanner_is_at_end()) {
		return make_token(EOF);
	}

	char c = scanner_advance();
	if (is_alpha(c)) return scanner_identifier();
	if (is_digit(c)) return scanner_number();

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
		case '!': return make_token(scanner_match('=') ? BANG_EQUAL : BANG);
		case '=': return make_token(scanner_match('=') ? EQUAL_EQUAL : EQUAL);
		case '<': return make_token(scanner_match('=') ? LESS_EQUAL : LESS);
		case '>': return make_token(scanner_match('=') ? GREATER_EQUAL : GREATER);
		case '"': return scanner_string();
	}

	return error_token("Unexpected character.");
}