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
	if (catch vm_err = vm_interpret(con)) {
		if (vm_err == COMPILE_ERROR) os::exit(65);
		if (vm_err == RUNTIME_ERROR) os::exit(65);
	}
}

fn void repl() => @pool() {
	io::printn("-----------------------------------------");
	io::printn("                 LOX REPL                ");
	io::printn("-----------------------------------------");

	for (;;) {
		io::printf(">>> ");
		String line = io::treadline()!!;
		if (line == "exit" || line == ":q" || line == "quit") {
			return;
		}

		if (try vm_interpret(line)) {
		}
	}
}

fn int main(String[] args) {
	if (args.len == 1) {
		repl();
	} else if (args.len == 2) {
		run_file(args[1]);
	} else {
		io::eprintf("TOO MANY ARGS: Can only take 1 arg");
		os::exit(64);
	}

	return 0;
}