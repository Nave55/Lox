module c3lox;

import std::io;

const STACK_MAX = 256;

struct Vm {
	Chunk* chunk;
	Uint8* ip;
	Value[STACK_MAX] stack;
	Value* stack_top;
}

Vm vm;

excuse COMPILE_ERROR, RUNTIME_ERROR;

fn void Vm.init(Vm* vm) {
	vm.reset_stack();
}

fn void Vm.free(Vm* vm) {
}

fn void Vm.reset_stack(Vm* vm) {
	vm.stack_top = &vm.stack;
}

fn Uint8 _read_byte() => *vm.ip++;
fn Value _read_constant() => vm.chunk.constants.values[_read_byte()];

fn void _binary_op(BinaryFn f) {
	double b = vm.pop().val.as_num;
	double a = vm.pop().val.as_num;
	vm.push({ (ValueData) { .as_num = f(a, b) }, NUMBER });
}

fn void? Vm.run(Vm* vm) {
	for (;;) {
		$if $feat(DEBUG_TRACE_EXECUTION):
			io::printf("          ");
			for (Value* slot = &vm.stack; slot < vm.stack_top; slot++) {
				io::printf("[ ");
				print_value(*slot);
				io::printf(" ]");
			}
			io::printn();
			vm.chunk.disassemble_instruction(vm.ip - &vm.chunk.code[0]);
		$endif

		Uint8 instruction;
		switch (instruction = _read_byte()) {
			case Op.CONSTANT.ordinal:
				Value constant = _read_constant();
				vm.push(constant);
			case Op.ADD.ordinal:
				_binary_op(fn (a, b) => a + b);
			case Op.SUBTRACT.ordinal:
				_binary_op(fn (a, b) => a - b);
			case Op.MULTIPLY.ordinal:
				_binary_op(fn (a, b) => a * b);
			case Op.DIVIDE.ordinal:
				_binary_op(fn (a, b) => a / b);
			case Op.NEGATE.ordinal:
				Value val = vm.pop();
				if (val.tag == NUMBER) {
					val.val.as_num = -val.val.as_num;
					vm.push(val);
				}
			case Op.RETURN.ordinal:
				print_value(vm.pop());
				io::printn();
				return;
		}
	}
}

fn void? Vm.interpret(Vm* vm, Chunk* chunk) {
	vm.chunk = chunk;
	vm.ip = &vm.chunk.code[0];
	return vm.run();
}

fn void Vm.push(Vm* vm, Value value) {
	*vm.stack_top = value;
	vm.stack_top++;
}

fn Value Vm.pop(Vm* vm) {
	vm.stack_top--;
	return *vm.stack_top;
}