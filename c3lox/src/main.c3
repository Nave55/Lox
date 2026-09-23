module c3lox;

import std::io;

fn void run_file(String path) {
	char[] con = load_file(path)!!;
	// io::printfn("%s", (String)con);

	Chunk chunk;
	chunk.init();

	ValueData val;
	val.as_num = 1.2;
	Uint8 constant = chunk.add_constant({ val, NUMBER });
	chunk.write(Op.CONSTANT.ordinal, 123);
	chunk.write(constant, 123);
	chunk.write(Op.RETURN.ordinal, 123);
	chunk.disassemble_chunk("test chunk");
}

fn void repl() {
	io::printn("Repl Placeholder");
}

fn void main(String[] args) {
	if (args.len == 1) {
		repl();
	} else if (args.len == 2) {
		run_file(args[1]);
	} else {
		io::printn("TOO MANY ARGS: Can only take 1 arg");
	}
}