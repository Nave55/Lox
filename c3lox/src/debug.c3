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

	// c3fmt off
	Uint8 instruction = chunk.code[offset];
	switch (instruction) {
		case Op.CONSTANT:      return constant_instruction("CONSTANT", chunk, offset);
		case Op.NIL:           return simple_instruction("NIL", offset);
		case Op.TRUE:          return simple_instruction("TRUE", offset);
		case Op.FALSE:         return simple_instruction("FALSE", offset);
		case Op.POP:           return simple_instruction("POP", offset);
		case Op.GET_GLOBAL:    return constant_instruction("GET_GLOBAL", chunk, offset);
		case Op.DEFINE_GLOBAL: return constant_instruction("DEFINE_GLOBAL", chunk, offset);
    case Op.SET_GLOBAL:    return constant_instruction("SET_GLOBAL", chunk, offset);
		case Op.EQUAL:         return simple_instruction("EQUAL", offset);
		case Op.GREATER:       return simple_instruction("GREATER", offset);
		case Op.LESS:          return simple_instruction("LESS", offset);
		case Op.ADD:           return simple_instruction("ADD", offset);
		case Op.SUBTRACT:      return simple_instruction("SUBTRACT", offset);
		case Op.MULTIPLY:      return simple_instruction("MULTIPLY", offset);
		case Op.DIVIDE:        return simple_instruction("DIVIDE", offset);
		case Op.NOT:           return simple_instruction("NOT", offset);
		case Op.NEGATE:        return simple_instruction("NEGATE", offset);
		case Op.PRINT:         return simple_instruction("PRINT", offset);
		case Op.RETURN:        return simple_instruction("RETURN", offset);
		default:
			io::printf("Unknown opcode %s\n", instruction);
			return offset + 1;
	}
}
// c3fmt on

fn sz simple_instruction(String name, sz offset) {
	io::printn(name);
	return offset + 1;
}

fn sz constant_instruction(String name, Chunk* chunk, sz offset) {
	Uint8 constant = chunk.code[offset + 1];
	io::printf("%-16s %4d '", name, constant);
	print_value(chunk.constants[constant]);
	io::printf("'\n");
	return offset + 2;
}

fn void print_value(Value val) {
	switch (val.tag) {
		case NUMBER: io::printf("%g", val.val.as_num);
		case BOOL: io::printf("%s", val.val.as_bool);
		case OBJ: print_object(val);
		default: io::printf("DEFAULT");
	}
}
