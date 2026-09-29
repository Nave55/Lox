module c3lox;

import std::collections::list;
import std::io;

excuse TAG_MISMATCH;

enum ValueType : Uint8 {
	BOOL,
	NUMBER,
	NIL,
	OBJ,
}

union ValueData {
	bool as_bool;
	double as_num;
	Obj* as_obj;
}

struct Value {
	ValueData val;
	ValueType tag;
}

alias ValueArray = List{Value};

fn sz ValueArray.write(ValueArray* array, Value val) {
	sz count = array.size;
	array.push(val);

	return count;
}

macro number_val(v) => (Value) { (ValueData) { .as_num = v }, NUMBER };
macro bool_val(v) => (Value) { (ValueData) { .as_bool = v }, BOOL };
macro nil_val() => (Value) { (ValueData) { .as_num = 0 }, NIL };
macro obj_val(v) => (Value) { (ValueData) { .as_obj = v }, OBJ };

macro is_num(v) => v.tag == NUMBER;
macro is_bool(v) => v.tag == BOOL;
macro is_nil(v) => v.tag == NIL;
macro is_obj(v) => v.tag == OBJ;

macro as_num(v) => v.val.as_num;
macro as_bool(v) => v.val.as_bool;
macro as_obj(v) => v.val.as_obj;

fn bool values_equal(Value a, Value b) {
	if (a.tag != b.tag) return false;
	switch (a.tag) {
		case BOOL: return as_bool(a) == as_bool(b);
		case NIL: return true;
		case NUMBER: return (as_num(a) - as_num(b)) < 1e-9;
		case OBJ: return as_string(a).equals(as_string(b));
		default: return false;  // Unreachable.
	}
}
