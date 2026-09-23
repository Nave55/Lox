module c3lox;

import std::io;

fn void Chunk.disassemble_chunk(Chunk* chunk, String name) {
	io::printf("== %s ==\n", name);

	for (sz offset = 0; offset < chunk.code.size;) {
		offset = chunk.disassemble_instruction(offset);
	}
}

fn sz Chunk.disassemble_instruction(Chunk* chunk, sz offset) {
	io::printf("%04d ", offset);
	if (offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1]) {
		io::printf("   | ");
	} else {
		io::printf("%4d ", chunk.lines[offset]);
	}

	Uint8 instruction = chunk.code[offset];
	switch (instruction) {
		case Op.CONSTANT.ordinal:
			return chunk.constant_instruction("CONSTANT", offset);
		case Op.RETURN.ordinal:
			return simple_instruction("RETURN", offset);
		default:
			io::printf("Unknown opcode %s\n", instruction);
			return offset + 1;
	}
}

fn sz simple_instruction(String name, sz offset) {
	io::printn(name);
	return offset + 1;
}

fn sz Chunk.constant_instruction(Chunk* chunk, String name, sz offset) {
	Uint8 constant = chunk.code[offset + 1];
	io::printf("%-16s %4d '", name, constant);
	print_value(chunk.constants.values[constant]);
	io::printf("'\n");
	return offset + 2;
}

fn void print_value(Value val) {
	switch (val.tag) {
		case NUMBER:
			io::printf("%g", val.val.as_num);
		case BOOL:
			io::printf("%s", val.val.as_bool);
		case STR:
			io::printf("%s", val.val.as_str);
		default:
			io::printn("DEFAULT");
	}
}