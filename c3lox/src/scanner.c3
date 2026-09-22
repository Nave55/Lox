module c3lox;

import std::io::file;

fn char[]? load_file(Allocator alloc, String filename) {
	return file::load(alloc, filename)!;
}