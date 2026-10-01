module c3lox;

fn void free_vm() {
	free_objects();
}

fn void free_objects() {
	foreach (i : vm.objects) free_object(i);
}

fn void free_object(Obj* object) {
	switch (object.tag) {
		case STRING: object.data.as_string.free();
	}
}

macro mem_free(type, pointer) => realloc(pointer, @sizeof(type));
