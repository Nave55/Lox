module c3lox;

import std::io;

const STACK_MAX = 256;

struct Vm {
	Chunk* chunk;
	Uint8* ip;
	Value[STACK_MAX] stack;
	Value* stack_top;
}

excuse COMPILE_ERROR, RUNTIME_ERROR;

Vm vm;

fn void Vm.init(Vm* vm) {
	vm.reset_stack();
}

fn void Vm.free(Vm* vm) {
}

fn void Vm.reset_stack(Vm* vm) {
	vm.stack_top = &vm.stack;
}

macro _read_byte() {
	return *vm.ip++;
}

macro _read_constant() {
	return vm.chunk.constants.values[_read_byte()];
}

macro _binary_op($op) {
	double b = vm.pop().val.as_num;
	double a = vm.pop().val.as_num;
	double r;

	$switch $op:
		$case "+":
			r = a + b;
		$case "-":
			r = a - b;
		$case "*":
			r = a * b;
		$case "/":
			r = a / b;
	$endswitch

	vm.push({ (ValueData) { .as_num = r }, NUMBER });
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
				_binary_op("+");
			case Op.SUBTRACT.ordinal:
				_binary_op("-");
			case Op.MULTIPLY.ordinal:
				_binary_op("*");
			case Op.DIVIDE.ordinal:
				_binary_op("/");
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