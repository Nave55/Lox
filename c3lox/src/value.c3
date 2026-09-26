module c3lox;

import std::collections::list;

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

struct ValueArray {
	List{Value} values;
}

macro @to_value(#val) {
	$switch $Typeof(#val):
		$case double:
			return (Value) { (ValueData) { .as_num = #val }, NUMBER };
		$case bool:
			return (Value) { (ValueData) { .as_bool = #val }, BOOL };
		$case string:
			return (Value) { (ValueData) { .as_str = #val }, STR };
		$default:
			(Value) { };
	$endswitch
}

fn void ValueArray.tinit(ValueArray* array) {
	array.values.tinit();
}

fn sz ValueArray.write(ValueArray* array, Value val) {
	sz count = array.values.size;
	array.values.push(val);

	return count;
}

// only use if allocated without tmem
fn void ValueArray.free(ValueArray* array) {
	array.values.free();
}