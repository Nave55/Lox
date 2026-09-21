module c3lox;

import std::io;

fn void run_file(String path) {
	char[] con = load_file(path)!!;
	foreach (i : con) io::printf("%c", i);
	io::printn();
}


fn void repl() {

}

fn void main(String[] args) {
	if (args.len == 1) {
		repl();
	} else if (args.len == 2) {
		run_file(args[1]);
	} else {
        io::printn("TOO MANY ARGS: Can only take 1 arg");
    }
}
