module c3lox;

import std::io;

const STACK_MAX = 256;

struct Vm {
	Chunk* chunk;
	Uint8* ip;
	Value[STACK_MAX] stack;
	Value* stack_top;
}

enum InterpretResult {
	OK,
	COMPILE_ERROR,
	RUNTIME_ERROR,
}

Vm vm;

fn void Vm.init(Vm* vm) {
	vm.reset_stack();
}

fn void Vm.free(Vm* vm) {
}

fn void Vm.reset_stack(Vm* vm) {
	vm.stack_top = &vm.stack;
}


macro read_byte() {
	return *vm.ip++;
}

macro read_constant() {
	return vm.chunk.constants.values[read_byte()];
}

macro binary_op($op) {
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

fn InterpretResult Vm.run(Vm* vm) {
	for (;;) {
		$if $defined(DEBUG_TRACE_EXECUTION):
			io::printf("          ");
			for (Value* slot = &vm.stack; slot < vm.stacktop; slot++) {
				io::printf("[ ");
				print_value(*slot);
				printf(" ]");
			}
			io::printn();
			vm.chunk.disassemble_instruction(vm.ip - vm.chunk.code);
		$endif
		Uint8 instruction;
		switch (instruction = read_byte()) {
			case Op.CONSTANT.ordinal:
				Value constant = read_constant();
				vm.push(constant);
			case Op.ADD.ordinal:
				binary_op("+");
			case Op.SUBTRACT.ordinal:
				binary_op("-");
			case Op.MULTIPLY.ordinal:
				binary_op("*");
			case Op.DIVIDE.ordinal:
				binary_op("/");
			case Op.NEGATE.ordinal:
				Value val = vm.pop();
				if (val.tag == NUMBER) {
					val.val.as_num = -val.val.as_num;
					vm.push(val);
				}
			case Op.RETURN.ordinal:
				print_value(vm.pop());
				io::printn();
				return OK;
		}
	}
}

fn InterpretResult Vm.interpret(Vm* vm, Chunk* chunk) {
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