module c3lox;

import std::io;

const STACK_MAX = 256;

struct Vm {
	Value[STACK_MAX] stack;
	Chunk* chunk;
	sz ip;
	sz stack_top;
}

Vm vm;

excuse COMPILE_ERROR, RUNTIME_ERROR;

fn void vm_init() {
	vm_reset_stack();
}

fn void vm_free() {
}

fn void vm_reset_stack() {
	vm.stack_top = 0;
	vm.ip = 0;
	vm.chunk = null;
}

fn Uint8 read_byte() {
	Uint8 byte = vm.chunk.code[vm.ip];
	vm.ip++;
	return byte;
}

fn Value read_constant() {
	return vm.chunk.constants.values[read_byte()];
}

fn void binary_op(BinaryFn f) {
	double b = vm_pop().val.as_num;
	double a = vm_pop().val.as_num;

	vm_push(
		{
			(ValueData) { .as_num = f(a, b) },
			NUMBER,
		}
	);
}

fn void? vm_run() {
	for (;;) {
		$if $feat(DEBUG_TRACE_EXECUTION):
			io::printf("          ");
			for (sz i = 0; i < vm.stack_top; i++) {
				io::printf("[ ");
				print_value(vm.stack[i]);
				io::printf(" ]");
			}
			io::printn();
			vm.chunk.disassemble_instruction(vm.ip);
		$endif

		Uint8 instruction = read_byte();
		switch (instruction) {
			case Op.CONSTANT.ordinal:
				Value constant = read_constant();
				vm_push(constant);

			case Op.ADD.ordinal: binary_op(fn (a, b) => a + b);
			case Op.SUBTRACT.ordinal: binary_op(fn (a, b) => a - b);
			case Op.MULTIPLY.ordinal: binary_op(fn (a, b) => a * b);
			case Op.DIVIDE.ordinal: binary_op(fn (a, b) => a / b);

			case Op.NEGATE.ordinal:
				Value value = vm_pop();
				if (value.tag != NUMBER) return RUNTIME_ERROR~;
				value.val.as_num = -value.val.as_num;
				vm_push(value);

			case Op.RETURN.ordinal:
				print_value(vm_pop());
				io::printn();
				return;
		}
	}
}

fn void? vm_interpret(String source) {
	compile(source);
	return;
}

fn void vm_push(Value value) {
	vm.stack[vm.stack_top] = value;
	vm.stack_top++;
}

fn Value vm_pop() {
	vm.stack_top--;
	return vm.stack[vm.stack_top];
}