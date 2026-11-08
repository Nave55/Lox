module c3lox;

import std::collections::list;

constset Op : inline Uint8 {
	CONSTANT = 0,
	NIL,
	TRUE,
	FALSE,
	POP,
	GET_LOCAL,
	SET_LOCAL,
	GET_GLOBAL,
	SET_GLOBAL,
	DEFINE_GLOBAL,
	EQUAL,
	GREATER,
	LESS,
	ADD,
	SUBTRACT,
	MULTIPLY,
	DIVIDE,
	MODULO,
	NOT,
	NEGATE,
	PRINT,
	JUMP,
	JUMP_IF_FALSE,
	JUMP_IF_TRUE,
	LOOP,
	RETURN,
}

struct Chunk {
	List{Uint8} code;
	List{sz} lines;
	ValueArray constants;
}

// ** allocations **
fn void Chunk.init(Chunk* chunk, Allocator alloc) {
	chunk.code.init(alloc, 32);
	chunk.lines.init(alloc, 16);
	chunk.constants.init(alloc, 16);
}

// free for chunks allocated using heap allocator
fn void Chunk.dealloc(Chunk* chunk) {
	chunk.code.free();
	chunk.lines.free();
	chunk.constants.free();
}

fn void Chunk.clear(Chunk* chunk) {
	chunk.code.clear();
	chunk.lines.clear();
	chunk.constants.clear();
}

fn void Chunk.write(Chunk* chunk, Uint8 byte, sz line) {
	chunk.code.push(byte);
	chunk.lines.push(line);
}

fn sz Chunk.count(Chunk* chunk) {
	return chunk.lines.len();
}

fn sz Chunk.add_constant(Chunk* chunk, Value val) {
	return chunk.constants.write(val);
}
