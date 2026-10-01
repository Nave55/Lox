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

fn Obj* allocate_obj(ObjType type) {
	Obj* obj = tmalloc(Obj::size);
	obj.tag = type;

	vm.objects.push(obj);
	return obj;
}

fn Obj* allocate_string(String str) {
	Obj* obj = allocate_obj(STRING);
	obj.data.as_string = dstring::new(tmem, str);

	return obj;
}

fn Obj* allocate_ident(String str) {
	Obj* obj = allocate_obj(IDENT);
	obj.data.as_ident = str;

	return obj;
}

fn void print_object(Value val) {
	switch (obj_type(val)) {
		case STRING: io::printf("%s", as_string(val));
		case IDENT: io::printf("%s", as_ident(val));
	}
}
