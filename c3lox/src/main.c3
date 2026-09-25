module c3lox;

import std::io;
import std::os;

fn void run_file(String path) => @pool() {
	char[]? con = file::load_temp(path);
	if (catch err = con) {
		io::eprintfn("Could not open file '%s'", path);
		os::exit(74);
	} else {
		vm_init();

		Chunk chunk;
		chunk.init();

		compile(con);
	}
}

fn void repl() => @pool() {
	io::printn("-----------------------------------------");
	io::printn("                 LOX REPL                ");
	io::printn("-----------------------------------------");

	for (;;) {
		io::printf(">>> ");
		String line = io::treadline()!!;
		if (line == "exit") {
			return;
		}
		io::printn(line);
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