module c3lox;

import std::io;
import std::os;

fn void run_file(String path) => @pool() {
	char[]? con = file::load_temp(path);
	if (catch con) {
		io::eprintfn("Could not open file '%s'", path);
		os::exit(74);
	}

	vm_init();
	interpret(con) ?? os::exit(65);
}

fn void repl() {
	io::printn("-----------------------------------------");
	io::printn("                 LOX REPL                ");
	io::printn("-----------------------------------------");

	for (;;) {
		@pool() {
			io::printf(">>> ");
			String line = io::treadline()!!;
			if (line == "cls") {
				io::print("\x1b[H\x1b[2J");
				continue;
			}
			if (line == ":q" || line == "exit") return;
			@catch(interpret(line));
		};
	}
}

fn int main(String[] args) {
	if (args.len == 1) {
		repl();
	} else if (args.len == 2) {
		run_file(args[1]);
	} else {
		io::eprintn("TOO MANY ARGS: Can only take 1 arg");
		os::exit(64);
	}

	return 0;
}
