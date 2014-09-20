"""
This module contains predefined functions. The set functions contains functions specifying the result of a function
and uses symbol names as keys. Each function takes as parameters the name of the function and a list of Atom objects
representing the parameters passed to the function.
"""

from __future__ import print_function
import re

import Atom
import Cons
import Environment


functions = {}


def eval(args, called_from):
	if not isinstance(args, Atom.Atom) and not isinstance(args, Cons.Cons):
		return args

	if isinstance(args, Cons.Cons):
		args = args.first
	atom = Atom.evaluate(args, called_from)
	if isinstance(atom, Atom.Atom):
		if atom.type == Atom.Atom.SYMBOL:
			return eval(Environment.get_symbol_value(atom.data), called_from)
		atom = Atom.evaluate(atom, called_from)

	return atom


def symbol_setf(fun, args):
	"""Sets the value of an atom

	ref		A reference to an atom
	value	The value to be evaluated and copied into ref"""

	ref = Atom.evaluate(args[0], fun, True)
	value = Atom.evaluate(args[1], fun, True)
	if not isinstance(value, Atom.Atom):
		new_atom = Atom.make_atom(value)
	else:
		new_atom = value

	ref.type = new_atom.type
	ref.data = new_atom.data
	return new_atom
functions["SETF"] = symbol_setf


def symbol_setq(fun, args):
	"""Sets a variable in the root stack frame"""

	symbol_name = args[0].data
	result = Atom.evaluate(args[1], fun)
	Environment.set_symbol(symbol_name.upper(), result)
	return result
functions["SETQ"] = symbol_setq


def symbol_defvar(fun, args):
	"""Sets a variable in the current stack frame"""

	symbol_name = args[0].data
	result = Atom.evaluate(args[1], fun)
	Environment.set_dynamic_symbol(symbol_name.upper(), result)
	return result
functions["DEFVAR"] = symbol_defvar


def evaluate_function(fun_name, args, params, results):
	Environment.enter_frame()

	# initialize parameters
	for i, arg in enumerate(args):
		param_name = params[i].data
		param_value = Atom.evaluate(arg, fun_name)  # eval(arg, fun_name)
		Environment.set_dynamic_symbol(param_name,
										param_value if isinstance(param_value, Atom.Atom) else Atom.make_atom(param_value))

	result = None
	for x in results:
		# evaluate result of function given current stack frame
		result = eval(x, fun_name)

	Environment.leave_frame()
	return result


def symbol_defun(fun, args):
	symbol_name = args[0].data
	function_args = Atom.cons_to_list(args[1].data)
	if function_args == [None]:
		function_args = []
	results = args[2:]
	functions[symbol_name] = lambda fun_name, args: evaluate_function(fun_name, args, function_args, results)
	return Atom.make_symbol(symbol_name)
functions["DEFUN"] = symbol_defun


def symbol_lambda(fun, args):
	function_args = Atom.cons_to_list(args[0].data)
	results = args[1:]
	return Atom.Atom(Atom.Atom.FUNCTION,
					lambda fun_name, fun_params: evaluate_function("<lambda>", fun_params, function_args, results))
functions["LAMBDA"] = symbol_lambda


def symbol_funcall(fun, args):
	fun_name = args[0]
	if fun_name.type == Atom.Atom.CONS:
		result = Atom.evaluate(fun_name, fun)
		return result.data(args[1:])
	else:
		function = Atom.evaluate(fun_name, fun)
		return function.data(function.__str__(), args[1:])
functions["FUNCALL"] = symbol_funcall


def symbol_let(fun, args):
	Environment.enter_frame()
	params = Atom.cons_to_list(args[0].data)
	result = args[1]

	for pair in params:
		pair = pair.data
		symbol_name = pair.first.data
		symbol_value = Atom.evaluate(pair.second.data.first, fun)
		Environment.set_dynamic_symbol(symbol_name, symbol_value)

	result = Atom.evaluate(result, fun)
	Environment.leave_frame()
	return result
functions["LET"] = symbol_let


def symbol_print(fun, args):
	for x in args:
		item = Atom.evaluate(x, fun)
		print(item, end="\n")
functions["PRINT"] = symbol_print


def symbol_format(fun, args):
	newline = Atom.evaluate(args[0], fun)
	#control = eval(args[1], fun)
	control = re.sub("\\{\\}", "%s", eval(args[1], fun))
	args = args[2:]
	for i in range(len(args)):
		args[i] = Atom.evaluate(args[i])
		if not isinstance(args[i], Atom.Atom):
			args[i] = Atom.make_atom(args[i])
		elif args[i].type == Atom.Atom.STRING:
			args[i] = args[i].data
	args = tuple(args)
	print(control % args, end="")
	if newline:
		print("")
functions["FORMAT"] = symbol_format


def symbol_exit(fun, args):
	if args:
		code = eval(args[0], fun)
	else:
		code = 0
	exit(code)
functions["EXIT"] = symbol_exit


def symbol_help(fun, args):
	for key in sorted(functions.keys()):
		print(key)
functions["HELP"] = symbol_help


def symbol_eval(fun, args):
	return Atom.evaluate(args[0], "EVAL")
functions["EVAL"] = symbol_eval

import fun_file
import fun_logic
import fun_math
import fun_sequence