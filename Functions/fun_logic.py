import Atom
import Environment
from Functions import eval, functions


def symbol_null(fun, args):
	result = Atom.evaluate(args[0], fun)
	if ((result.type == Atom.Atom.BOOLEAN and result.data is False) or
				(not result and type(result) is bool)):
		return Atom.T
	else:
		return Atom.NIL


functions["NULL"] = symbol_null


def symbol_and(fun, args):
	for x in args:
		condition = eval(x, fun)
		if not condition:
			return Atom.NIL
	return Atom.T


functions["AND"] = symbol_and


def symbol_or(fun, args):
	for x in args:
		condition = eval(x, fun)
		if condition:
			return Atom.T
	return Atom.NIL


functions["OR"] = symbol_or


def symbol_if(fun, args):
	condition = eval(args[0], fun)
	if condition:
		return Atom.evaluate(args[1], fun)
	option2 = Atom.evaluate(args[2], fun) if len(args) > 2 else Atom.NIL
	return option2


functions["IF"] = symbol_if


def symbol_while(fun, args):
	# condition = args[0]
	#statements = args[1:]
	result = Atom.NIL
	while True:
		#if not eval(condition, fun):
		#	return result
		for s in args:
			result = Atom.evaluate(s, fun)
			if isinstance(result, Atom.Atom) and result.type == Atom.Atom.CONS and result.data.first.data == "RETURN":
				if result.data.second != Atom.NIL:
					return Atom.evaluate(result.data.second.data.first)
				else:
					return Atom.NIL


functions["WHILE"] = symbol_while


def symbol_do(fun, args):
	condition = args[0]
	body = args[1:]
	result = Atom.NIL
	while True:
		if not eval(condition, fun):
			return result
		for s in body:
			result = Atom.evaluate(s, fun)
	return result


functions["DO"] = symbol_do


def symbol_dolist(fun, args):
	pair = Atom.cons_to_list(args[0].data)
	symbol_name = pair[0].data
	list = Atom.cons_to_list(Atom.evaluate(pair[1]).data)

	Environment.enter_frame()

	body = args[1:]
	result = None
	for x in list:
		Environment.set_dynamic_symbol(symbol_name, x)
		for s in body:
			result = Atom.evaluate(s)

	Environment.leave_frame()
	return result


functions["DOLIST"] = symbol_dolist


def symbol_cond(fun, args):
	for x in args:
		variant = x.data
		sub = variant.second.data
		condition = eval(variant.first, fun)
		if condition:
			return eval(sub, fun)
	return Atom.NIL


functions["COND"] = symbol_cond