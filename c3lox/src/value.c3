module c3lox;

import std::collections::list;
import std::io;

excuse TAG_MISMATCH;

enum ValueType : Uint8 {
	BOOL,
	NUMBER,
	NIL,
	STR,
	FUNC,
	NATIVE,
	CLOSURE,
	CLASS,
	INSTANCE,
	BOUND
}

union ValueData {
	bool as_bool;
	double as_num;
	String as_str;
	// Func*   as_func;
	// Native* as_native;
	// Closure* as_closure;
	// Class* as_class;
	// Instance* as_instance;
	// BoundMethod* as_bound;
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

macro @to_value(#val) {
	$switch $Typeof(#val):
		$case double:
			return (Value) { (ValueData) { .as_num = #val }, NUMBER };
		$case bool:
			return (Value) { (ValueData) { .as_bool = #val }, BOOL };
		$case String:
			return (Value) { (ValueData) { .as_str = #val }, STR };
		$case ValueType:
			$if #val == NIL:
				return (Value) { (ValueData) { .as_num = 0d }, NIL };
			$endif
		$default:
			io::printn("@to_value - HIT DEFAULT");
			(Value) { };
	$endswitch
}

macro is_num(v) {
	return v.tag == NUMBER;
}

macro is_bool(v) {
	return v.tag == BOOL;
}

macro is_str(v) {
	return v.tag == STR;
}

macro is_nil(v) {
	return v.tag == NIL;
}

macro as_num(v) {
	return v.val.as_num;
}

macro as_bool(v) {
	return v.val.as_bool;
}

macro as_str(v) {
	return v.val.as_str;
}

fn bool values_equal(Value a, Value b) {
	if (a.tag != b.tag) return false;
	switch (a.tag) {
		case BOOL: return as_bool(a) == as_bool(b);
		case NIL: return true;
		case NUMBER: return (as_num(a) - as_num(b)) < 1e-9;
		default: return false;  // Unreachable.
	}
}
