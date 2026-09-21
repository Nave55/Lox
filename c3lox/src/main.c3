module c3lox;

import std::io;

fn void main(String[] args) {;
	char[] con = load_file("resources/input.txt")!!;
	foreach (i : con) io::printf("%c", i);
	io::printn();
}
