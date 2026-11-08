module c3lox;

import std::collections::list;
import std::math;

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

macro Value number_val(v) => (Value) { (ValueData) { .as_num = v }, NUMBER };
macro Value bool_val(v) => (Value) { (ValueData) { .as_bool = v }, BOOL };
macro Value nil_val() => (Value) { (ValueData) { .as_num = 0 }, NIL };
macro Value obj_val(v) => (Value) { (ValueData) { .as_obj = v }, OBJ };

macro bool is_num(v) => v.tag == NUMBER;
macro bool is_bool(v) => v.tag == BOOL;
macro bool is_nil(v) => v.tag == NIL;
macro bool is_obj(v) => v.tag == OBJ;

macro double as_num(v) => v.val.as_num;
macro bool as_bool(v) => v.val.as_bool;
macro Obj* as_obj(v) => v.val.as_obj;

fn bool values_equal(Value a, Value b) {
	if (a.tag != b.tag) return false;
	switch (a.tag) {
		case BOOL: return as_bool(a) == as_bool(b);
		case NIL: return true;
		case NUMBER: return math::abs(as_num(a) - as_num(b)) < 1e-9;
		case OBJ:
			switch (as_obj(a).tag) {
				case STRING: return as_string(a).equals(as_string(b));
			}
	}

	return false;
}
