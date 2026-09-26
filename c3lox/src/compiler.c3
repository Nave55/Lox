module c3lox;

import std::io;
import scanner;

fn void compile(char[] source) {
	scanner::init(source);
	sz line = -1;
	for (;;) {
		Token token = scanner::scan_token();
		if (token.line != line) {
			io::printf("%4d ", token.line);
			line = token.line;
		} else {
			io::printf("   | ");
		}

		io::printfn("%s '%s'", token.type, token.lexeme);

		if (token.type == EOF) {
			break;
		}
	}
}