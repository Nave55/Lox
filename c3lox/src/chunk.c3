module c3lox;

import std::collections::list;

constdef Op : inline Uint8 {
	CONSTANT = 0,
	NIL,
	TRUE,
	FALSE,
	EQUAL,
	GREATER,
	LESS,
	ADD,
	SUBTRACT,
	MULTIPLY,
	DIVIDE,
	NOT,
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
