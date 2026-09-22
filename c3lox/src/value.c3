module c3lox;

import std::collections::list;

struct ValueArray {
	List{Value} values;
}

fn void ValueArray.init(ValueArray* array, Allocator alloc = tmem) {
	array.values.init(alloc);
}

fn sz ValueArray.write(ValueArray* array, Value value) {
	sz count = array.values.size;
	array.values.push(value);

	return count;
}

// only use if allocated without tmem
fn void ValueArray.free(ValueArray* array) {
	array.values.free();
}