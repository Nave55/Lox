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

fn void vm_reset_stack() {
	vm.stack_top = 0;
	vm.ip = 0;
	vm.chunk = null;
}

fn void runtime_error(String format, args...) {
	io::eprintfn(format, args);

	sz instruction = vm.ip - 1;
	sz line        = vm.chunk.lines[instruction];
	io::eprintfn("[line %d] in script", line);
	vm_reset_stack();
}

fn Uint8 read_byte() {
	Uint8 byte = vm.chunk.code[vm.ip];
	vm.ip++;
	return byte;
}

fn Value read_constant() => vm.chunk.constants[read_byte()];

macro binary_fn(a, b, $op) {
	$switch ($op):
		$case '+':
			return a + b;
		$case '-':
			return a - b;
		$case '*':
			return a * b;
		$case '/':
			return a / b;
		$case '>':
			return a > b;
		$case '<':
			return a < b;
	$endswitch
}

macro void? @binary_op(#value_type, $op) {
	if (!is_num(peek(0)) || !is_num(peek(1))) {
		runtime_error("Operands must be numbers.");
		return RUNTIME_ERROR~;
	}

	double b = pop().val.as_num;
	double a = pop().val.as_num;
	push(#value_type(binary_fn(a, b, $op)));
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
			case Op.CONSTANT:
				Value constant = read_constant();
				push(constant);

			case Op.NIL: push(@to_value(ValueType.NIL));
			case Op.TRUE: push(@to_value(true));
			case Op.FALSE: push(@to_value(false));
			case Op.EQUAL:

				{
					Value b = pop();
					Value a = pop();
					push(@to_value(values_equal(a, b)));
				}

			case Op.GREATER: @binary_op(@to_value, '>')!;
			case Op.LESS: @binary_op(@to_value, '<')!;
			case Op.ADD: @binary_op(@to_value, '+')!;
			case Op.SUBTRACT: @binary_op(@to_value, '-')!;
			case Op.MULTIPLY: @binary_op(@to_value, '*')!;
			case Op.DIVIDE: @binary_op(@to_value, '/')!;
			case Op.NOT: push(@to_value(is_falsey(pop())));

			case Op.NEGATE:
				if (!is_num(peek(0))) {
					runtime_error("Operand must be a number");
					return RUNTIME_ERROR~;
				}
				push(@to_value(-as_num(pop())));
			case Op.RETURN:
				print_value(pop());
				io::printn();
				return;
		}
	}
}

fn void? interpret(char[] source) {
	Chunk chunk;
	chunk.init();

	if (!compile(source, &chunk)) return COMPILE_ERROR~;

	vm.chunk = &chunk;
	vm.ip = 0;

	return vm_run();
}

fn void push(Value value) {
	vm.stack[vm.stack_top] = value;
	vm.stack_top++;
}

fn Value pop() {
	vm.stack_top--;
	return vm.stack[vm.stack_top];
}

fn Value peek(sz distance) {
	return vm.stack[vm.stack_top - distance - 1];
}

fn bool is_falsey(Value value) {
	return is_nil(value) || (is_bool(value) && !as_bool(value));
}
