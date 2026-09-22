module c3lox;

import std::io;

fn void Chunk.disassemble_chunk(Chunk* chunk, String name) {
	io::printf("== %s ==\n", name);

	for (int offset = 0; offset < chunk.code.size;) {
		offset = chunk.disassemble_instruction(offset);
	}
}

fn Int32 Chunk.disassemble_instruction(Chunk* chunk, Int32 offset) {
	io::printf("%04d ", offset);
	if (offset > 0 && chunk.lines[offset] == chunk.lines[(sz)offset - 1]) {
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

fn Int32 simple_instruction(String name, Int32 offset) {
	io::printf("%s\n", name);
	return offset + 1;
}

fn Int32 Chunk.constant_instruction(Chunk* chunk, String name, Int32 offset) {
	Uint8 constant = chunk.code[(sz)offset + 1];
	io::printf("%-16s %4d '", name, constant);
	print_value(chunk.constants.values[constant]);
	io::printf("'\n");
	return offset + 2;
}

fn void print_value(Value value) {
	io::printf("%g", value);
}