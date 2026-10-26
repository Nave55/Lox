module c3lox;

import std::io;
import std::collections::list;
import std::collections::map;

excuse COMPILE_ERROR, RUNTIME_ERROR;

struct Vm {
	Value[STACK_MAX] stack;
	List{Obj*} objects;
	HashMap{String, Value} globals;
	Compiler compiler;
	Chunk chunk;
	sz ip;
	sz stack_top;
}

// allocations
fn void Vm.init(Vm* vm) {
	vm.chunk.init();
	vm.objects.tinit(32);
	vm.globals.tinit(16);
	vm.compiler.global_map.tinit(16);
	vm.ip = 0;
	vm.stack_top = 0;
}

fn void Vm.reset_stack(Vm* vm) {
	vm.stack_top = 0;
	vm.ip = 0;
}

fn void Vm.reset(Vm* vm) {
	vm.reset_stack();
	vm.chunk.clear();
}

fn void Vm.runtime_error(Vm* vm, String format, args...) {
	io::eprintfn(format, args);

	sz instruction = vm.ip - 1;
	sz line        = vm.chunk.lines[instruction];
	io::eprintfn("[line %d] in script", line);
	vm.reset_stack();
}

fn Uint8 Vm.read_byte(Vm* vm) {
	Uint8 byte = vm.chunk.code[vm.ip];
	vm.ip++;
	return byte;
}

macro Value Vm.read_constant(Vm* vm) => vm.chunk.constants[vm.read_byte()];
macro DString Vm.read_dstring(Vm* vm) => as_string(vm.read_constant());
macro String Vm.read_ident(Vm* vm) => as_ident(vm.read_constant());

macro read_short(Vm* vm) {
	vm.ip += 2;
	return (sz)(vm.chunk.code[vm.ip - 2] << 8 | vm.chunk.code[vm.ip - 1]);
}

// c3fmt off
macro binary_fn(a, b, $op) {
	$switch ($op):
		$case '+': return a + b;
		$case '-': return a - b;
		$case '*': return a * b;
		$case '/': return a / b;
		$case '%': return a % b;
		$case '>': return a > b;
		$case '<': return a < b;
	$endswitch
}
// c3fmt on

macro void? @binary_op(#vm, #value_type, $op) {
	if (!is_num(#vm.peek(0)) || !is_num(#vm.peek(1))) {
		#vm.runtime_error("Operands must be numbers.");
		return RUNTIME_ERROR~;
	}

	double b = #vm.pop().val.as_num;
	double a = #vm.pop().val.as_num;
	#vm.push(#value_type(binary_fn(a, b, $op)));
}

fn void? Vm.run(Vm* vm) {
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

		Uint8 instruction = vm.read_byte();
		switch (instruction) {
			case Op.CONSTANT:
				Value constant = vm.read_constant();
				vm.push(constant);
			case Op.NIL: vm.push(nil_val());
			case Op.TRUE: vm.push(bool_val(true));
			case Op.FALSE: vm.push(bool_val(false));
			case Op.POP: vm.pop();
			case Op.GET_LOCAL:
				Uint8 slot = vm.read_byte();
				vm.push(vm.stack[slot]);
			case Op.SET_LOCAL:
				Uint8 slot = vm.read_byte();
				vm.stack[slot] = vm.peek(0);
			case Op.GET_GLOBAL:
				String name = vm.read_ident();
				Value? value = vm.globals.get(name);
				if (catch value) {
					vm.runtime_error("Undefined variable '%s'.", name);
					return RUNTIME_ERROR~;
				}
				vm.push(value);
			case Op.DEFINE_GLOBAL:
				String name = vm.read_ident();
				vm.globals.set(name, vm.peek(0));
				vm.pop();
			case Op.SET_GLOBAL:
				String name = vm.read_ident();
				if (!vm.globals.set(name, vm.peek(0))) {
					vm.globals.remove(name);
					vm.runtime_error("Undefined variable '%s'.", name);
					return RUNTIME_ERROR~;
				}
			case Op.EQUAL:
				Value b = vm.pop();
				Value a = vm.pop();
				vm.push(bool_val(values_equal(a, b)));
			case Op.GREATER: @binary_op(vm, bool_val, '>')!;
			case Op.LESS: @binary_op(vm, bool_val, '<')!;
			case Op.ADD:
				if (is_string(vm.peek(0)) && is_string(vm.peek(1))) {
					vm.concatenate();
				} else if (is_num(vm.peek(0)) && is_num(vm.peek(1))) {
					@binary_op(vm, number_val, '+')!;
				} else {
					vm.runtime_error("Operands must be two numbers or two strings.");
					return RUNTIME_ERROR~;
				}
			case Op.SUBTRACT: @binary_op(vm, number_val, '-')!;
			case Op.MULTIPLY: @binary_op(vm, number_val, '*')!;
			case Op.DIVIDE: @binary_op(vm, number_val, '/')!;
			case Op.MODULO: @binary_op(vm, number_val, '%')!;
			case Op.NOT: vm.push(bool_val(is_falsey(vm.pop())));
			case Op.NEGATE:
				if (!is_num(vm.peek(0))) {
					vm.runtime_error("Operand must be a number");
					return RUNTIME_ERROR~;
				}
				vm.push(number_val(-as_num(vm.pop())));
			case Op.PRINT:
				print_value(vm.pop());
				io::printn();
			case Op.JUMP:
				sz offset = read_short(vm);
				vm.ip += offset;
			case Op.JUMP_IF_FALSE:
				sz offset = read_short(vm);
				if (is_falsey(vm.peek(0))) vm.ip += offset;
			case Op.JUMP_IF_TRUE:
				sz offset = read_short(vm);
				if (!is_falsey(vm.peek(0))) vm.ip += offset;
			case Op.LOOP:
				sz offset = read_short(vm);
				vm.ip -= offset;
			case Op.RETURN:
				return;
		}
	}
}

fn void? Vm.interpret(Vm* vm, char[] source) {
	vm.reset();
	if (!compile(vm, source)) return COMPILE_ERROR~;
	return vm.run();
}

fn void Vm.push(Vm* vm, Value value) {
	vm.stack[vm.stack_top] = value;
	vm.stack_top++;
}

fn Value Vm.pop(Vm* vm) {
	vm.stack_top--;
	return vm.stack[vm.stack_top];
}

fn Value Vm.peek(Vm* vm, sz distance) => vm.stack[vm.stack_top - distance - 1];
fn bool is_falsey(Value value) => is_nil(value) || (is_bool(value) && !as_bool(value));

fn void Vm.concatenate(Vm* vm) {
	DString b = as_string(vm.pop());
	Value a   = vm.pop();
	as_string(a).append(b);
	vm.push(a);
}
