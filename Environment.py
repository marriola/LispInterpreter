import collections

symbol_stack = collections.deque()


def enter_frame():
	"""Constructs a new active stack frame"""
	symbol_stack.appendleft({})


def leave_frame():
	"""Pops the current stack frame off the top"""
	symbol_stack.popleft()


def set_symbol(symbol, value):
	"""Sets a symbol in the root stack frame"""
	replace = collections.deque()
	while len(symbol_stack) > 1:
		stack_frame = symbol_stack.popleft()
		replace.appendleft(stack_frame)
	stack_frame = symbol_stack.popleft()
	stack_frame[symbol] = value
	symbol_stack.appendleft(stack_frame)
	while len(replace):
		symbol_stack.appendleft(replace.popleft())


def set_dynamic_symbol(symbol, value):
	"""Sets a symbol in the current stack frame"""
	stack_frame = symbol_stack.popleft()
	stack_frame[symbol] = value
	symbol_stack.appendleft(stack_frame)


def get_symbol_value(symbol):
	"""Searches the stack from the top down for a symbol"""
	for stack_frame in symbol_stack:
		if symbol in stack_frame:
			return stack_frame[symbol]
	return None

enter_frame()
import math
set_symbol("_PI", math.pi)
set_symbol("_E", math.e)