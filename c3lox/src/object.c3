module c3lox;

import std::io;

constset ObjType : inline Uint8 {
	STRING = 0,
	IDENT,
	FUNC,
	NATIVE,
	CLOSURE,
	CLASS,
	INSTANCE,
	BOUND
}

union ObjData {
	DString as_string;
	String as_ident;
}

struct Obj {
	ObjData data;
	ObjType tag;
}

macro obj_type(v) => as_obj(v).tag;
macro is_string(v) => is_obj(v) && obj_type(v) == STRING;
macro as_string(v) => as_obj(v).data.as_string;
macro is_ident(v) => is_obj(v) && obj_type(v) == IDENT;
macro as_ident(v) => as_obj(v).data.as_ident;

// allocation
fn Obj* Vm.allocate_obj(Vm* vm, ObjType type) {
	Obj* obj = malloc(Obj::size);
	obj.tag = type;

	vm.objects.push(obj);
	return obj;
}

fn Obj* Vm.allocate_string(Vm* vm, String str) {
	Obj* obj = vm.allocate_obj(STRING);
	obj.data.as_string = dstring::new(mem, str);

	return obj;
}

fn Obj* Vm.allocate_ident(Vm* vm, String str) {
	Obj* obj = vm.allocate_obj(IDENT);
	obj.data.as_ident = str;

	return obj;
}

fn void print_object(Value val) {
	switch (obj_type(val)) {
		case STRING: io::printf("%s", as_string(val));
		case IDENT: io::printf("%s", as_ident(val));
	}
}
