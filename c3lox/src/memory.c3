module c3lox;

fn void Vm.free(Vm* vm) {
	vm.free_objects();
}

fn void Vm.free_objects(Vm* vm) {
	foreach (i : vm.objects) i.dealloc();
	vm.objects.clear();
}

macro void* mem_alloc(size) => realloc(null, size);
macro void* mem_dealloc(pointer, size) => realloc(pointer, size);
