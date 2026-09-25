module c3lox;

import std::io;

fn void run_file(String path) => @pool() {
	char[] con = load_file(path)!!;
	// io::printfn("%s", (String)con);
	vm.init();

	Chunk chunk;
	chunk.init();

	Uint8 constant = chunk.add_constant(@to_value(1.2));
	chunk.write(Op.CONSTANT.ordinal, 123);
	chunk.write(constant, 123);

	constant = chunk.add_constant(@to_value(3.4));
	chunk.write(Op.CONSTANT.ordinal, 123);
	chunk.write(constant, 123);

	chunk.write(Op.ADD.ordinal, 123);

	constant = chunk.add_constant(@to_value(5.6));
	chunk.write(Op.CONSTANT.ordinal, 123);
	chunk.write(constant, 123);

	chunk.write(Op.DIVIDE.ordinal, 123);

	chunk.write(Op.NEGATE.ordinal, 123);

	chunk.write(Op.RETURN.ordinal, 123);
	chunk.disassemble_chunk("test chunk");

	InterpretResult res = vm.interpret(&chunk);
	io::printn(res);
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