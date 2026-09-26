module c3lox;

import std::collections::list;

enum Op : Uint8 {
	CONSTANT,
	ADD,
	SUBTRACT,
	MULTIPLY,
	DIVIDE,
	NEGATE,
	RETURN,
}

struct Chunk {
	List{Uint8} code;
	List{sz} lines;
	ValueArray constants;
}

fn void Chunk.init(Chunk* chunk) {
	chunk.code.tinit();
	chunk.lines.tinit();
	chunk.constants.tinit();
}

fn void Chunk.write(Chunk* chunk, Uint8 byte, sz line) {
	chunk.code.push(byte);
	chunk.lines.push(line);
}

fn sz Chunk.add_constant(Chunk* chunk, Value val) {
	return chunk.constants.write(val);
}