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

struct ObjFunction {
	String name;
	Chunk chunk;
	sz arity;
}

union ObjData {
	DString as_string;
	String as_ident;
	ObjFunction as_func;
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

macro is_func(v) => is_obj(v) && obj_type(v) == FUNC;
macro as_func(v) => as_obj(v).data.as_func;

// allocates object
fn Obj* Vm.allocate_obj(Vm* vm, ObjType type) {
	Obj* obj = (Obj*)mem_alloc(Obj::size);
	obj.tag = type;

	vm.objects.push(obj);
	return obj;
}

// allocates obj and string
fn Obj* Vm.allocate_string(Vm* vm, String str) {
	Obj* obj = vm.allocate_obj(STRING);
	obj.data.as_string = dstring::new(mem, str);

	return obj;
}

// allocates obj
fn Obj* Vm.allocate_ident(Vm* vm, String str) {
	Obj* obj = vm.allocate_obj(IDENT);
	obj.data.as_ident = str;

	return obj;
}

// allocates obj and chunk values
fn Obj* Vm.allocate_function(Vm* vm, Allocator alloc) {
	Obj* obj = vm.allocate_obj(FUNC);
	obj.data.as_func.chunk.init(alloc);

	return obj;
}

fn void Obj.dealloc(Obj* object) {
	switch (object.tag) {
		case STRING: object.data.as_string.free();
		case FUNC: object.data.as_func.chunk.dealloc();
	}

	mem_dealloc(object, 0);
}

fn void print_object(Value val) {
	switch (obj_type(val)) {
		case STRING: io::printf("%s", as_string(val));
		case IDENT: io::printf("%s", as_ident(val));
		case FUNC: io::printf("<fn %s>", as_func(val).name);
	}
}
