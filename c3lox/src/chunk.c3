module c3lox;

import std::collections::list;

enum Op : Uint8 {
	CONSTANT,
	RETURN,
}

struct Chunk {
	List{Uint8} code;
	List{Int32} lines;
	ValueArray constants;
}

fn void Chunk.init(Chunk* chunk, Allocator alloc = tmem) {
	chunk.code.init(alloc);
	chunk.lines.init(alloc);
}

fn void Chunk.write(Chunk* chunk, Uint8 byte, Int32 line) {
	chunk.code.push(byte);
	chunk.lines.push(line);
}

// only use if allocated without tmem
fn void Chunk.free(Chunk* chunk) {
	chunk.code.free();
	chunk.lines.free();
}

fn Uint8 Chunk.add_constant(Chunk* chunk, Value value) {
	return (Uint8)chunk.constants.write(value);
}