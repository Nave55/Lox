module c3lox;

import std::io;
import std::core::mem;

fn void run_file(String path) {
	char[] con = load_file(tmem, path)!!;
	// io::printfn("%s", (String)con);
	Chunk chunk;
	chunk.init();
	Uint8 constant = chunk.add_constant(1.2);
	chunk.write(Op.CONSTANT.ordinal, 123);
	chunk.write(constant, 123);

	chunk.write(Op.RETURN.ordinal, 123);
	chunk.disassemble_chunk("test chunk");
}

fn void repl() {
	io::printn("Repl Placeholder");
}

fn void main(String[] args) => @pool() {
	if (args.len == 1) {
		repl();
	} else if (args.len == 2) {
		run_file(args[1]);
	} else {
		io::printn("TOO MANY ARGS: Can only take 1 arg");
	}
}