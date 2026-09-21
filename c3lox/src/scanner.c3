module c3lox;

import std::io;
import std::io::file;

fn char[]? load_file(String filename) {
    File fh = file::open(filename, "rb")!;
    defer (void)fh.close();

    fh.seek(0, FROM_END)!;
    long len = fh.cursor()!;
    fh.seek(0, FROM_START)!;

    char* buffer = malloc(len);
    sz read = fh.read(buffer[:len])!;

    return buffer[:len];
}