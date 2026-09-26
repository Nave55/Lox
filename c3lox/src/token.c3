module c3lox;

enum TokenType : Uint8 (ParseRule* rule) {
	// Single-character tokens.
	LEFT_PAREN    { &&{ &compiler_grouping, null, Precedence.NONE } },
	RIGHT_PAREN   { &&{ null, null, Precedence.NONE } },
	LEFT_BRACE    { &&{ null, null, Precedence.NONE } },
	RIGHT_BRACE   { &&{ null, null, Precedence.NONE } },
	COMMA         { &&{ null, null, Precedence.NONE } },
	DOT           { &&{ null, null, Precedence.NONE } },
	MINUS         { &&{ &compiler_unary, &compiler_binary, Precedence.TERM } },
	PLUS          { &&{ null, &compiler_binary, Precedence.TERM } },
	SEMICOLON     { &&{ null, null, Precedence.NONE } },
	SLASH         { &&{ null, &compiler_binary, Precedence.FACTOR } },
	STAR          { &&{ null, &compiler_binary, Precedence.FACTOR } },
	// One or two character tokens.
	BANG          { &&{ null, null, Precedence.NONE } },
	BANG_EQUAL    { &&{ null, null, Precedence.NONE } },
	EQUAL         { &&{ null, null, Precedence.NONE } },
	EQUAL_EQUAL   { &&{ null, null, Precedence.NONE } },
	GREATER       { &&{ null, null, Precedence.NONE } },
	GREATER_EQUAL { &&{ null, null, Precedence.NONE } },
	LESS          { &&{ null, null, Precedence.NONE } },
	LESS_EQUAL    { &&{ null, null, Precedence.NONE } },
	// Literals.
	IDENTIFIER    { &&{ null, null, Precedence.NONE } },
	STRING        { &&{ null, null, Precedence.NONE } },
	NUMBER        { &&{ &compiler_number, null, Precedence.NONE } },
	// Keywords.
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
	// rest
	ERROR         { &&{ null, null, Precedence.NONE } },
	EOF           { &&{ null, null, Precedence.NONE } },
}

struct Token {
	TokenType type;
	String lexeme;
	sz line;
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