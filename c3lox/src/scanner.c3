module c3lox;

import std::io::file;

fn char[]? load_file(String filename, Allocator alloc = tmem) {
	return file::load(alloc, filename)!;
}