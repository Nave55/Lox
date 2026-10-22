module c3lox;

fn void Vm.free_vm(Vm* vm) {
	vm.free_objects();
}

fn void Vm.free_objects(Vm* vm) {
	foreach (i : vm.objects) free_object(i);
}

fn void free_object(Obj* object) {
	switch (object.tag) {
		case STRING: object.data.as_string.free();
	}
	free(object);
}

macro mem_free(type, pointer) => realloc(pointer, @sizeof(type));
