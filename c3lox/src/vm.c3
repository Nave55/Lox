module c3lox;

import std::io;
import std::collections::list;

excuse COMPILE_ERROR, RUNTIME_ERROR;
const STACK_MAX = 256;

struct Vm {
	Value[STACK_MAX] stack;
	List{Obj*} objects;
	Chunk* chunk;
	sz ip;
	sz stack_top;
}

Vm vm;

fn void vm_init() {
	vm_reset_stack();
	vm.objects.tinit(64);
}

fn void vm_reset_stack() {
	vm.stack_top = 0;
	vm.ip = 0;
	vm.chunk = null;
	vm.objects.clear();
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

// c3fmt off
macro binary_fn(a, b, $op) {
	$switch ($op):
		$case '+': return a + b;
		$case '-': return a - b;
		$case '*': return a * b;
		$case '/': return a / b;
		$case '>': return a > b;
		$case '<': return a < b;
	$endswitch
}
// c3fmt on

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

			case Op.NIL: push(nil_val());
			case Op.TRUE: push(bool_val(true));
			case Op.FALSE: push(bool_val(false));
			case Op.EQUAL:
				Value b = pop();
				Value a = pop();
				push(bool_val(values_equal(a, b)));

			case Op.GREATER: @binary_op(bool_val, '>')!;
			case Op.LESS: @binary_op(bool_val, '<')!;
			case Op.ADD:
				if (is_string(peek(0)) && is_string(peek(1))) {
					concatenate();
				} else if (is_num(peek(0)) && is_num(peek(1))) {
					@binary_op(number_val, '+')!;
				} else {
					runtime_error("Operands must be two numbers or two strings.");
					return RUNTIME_ERROR~;
				}
			case Op.SUBTRACT: @binary_op(number_val, '-')!;
			case Op.MULTIPLY: @binary_op(number_val, '*')!;
			case Op.DIVIDE: @binary_op(number_val, '/')!;
			case Op.NOT: push(bool_val(is_falsey(pop())));

			case Op.NEGATE:
				if (!is_num(peek(0))) {
					runtime_error("Operand must be a number");
					return RUNTIME_ERROR~;
				}
				push(number_val(-as_num(pop())));
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

	// fault err = @catch(vm_run());
	// free_vm();
	// return err~;
	
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

fn Value peek(sz distance) => vm.stack[vm.stack_top - distance - 1];

fn bool is_falsey(Value value) {
	return is_nil(value) || (is_bool(value) && !as_bool(value));
}

fn void concatenate() {
	DString b = as_string(pop());
	Value a   = pop();
	as_string(a).append(b);
	push(a);
}
