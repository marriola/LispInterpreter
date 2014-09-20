import math
from LispError import LispError
from Functions import eval, functions


def symbol_eq(fun, args):
	first = eval(args[0], fun)
	second = eval(args[1], fun)
	return first == second
functions["="] = symbol_eq


def symbol_lt(fun, args):
	first = eval(args[0], fun)
	second = eval(args[1], fun)
	return first < second
functions["<"] = symbol_lt


def symbol_lte(fun, args):
	first = eval(args[0], fun)
	second = eval(args[1], fun)
	return first <= second
functions["<="] = symbol_lte


def symbol_gt(fun, args):
	first = eval(args[0], fun)
	second = eval(args[1], fun)
	return first > second
functions[">"] = symbol_gt


def symbol_gte(fun, args):
	first = eval(args[0], fun)
	second = eval(args[1], fun)
	return first >= second
functions[">="] = symbol_gte


def symbol_plus(fun, args):
	sum = 0
	for x in args:
		x = eval(x, fun)
		sum += x
	return sum
functions["+"] = symbol_plus


def symbol_one_plus(fun, args):
	return eval(args[0], fun) + 1
functions["1+"] = symbol_one_plus


def symbol_minus(fun, args):
	sum = eval(args[0], fun)
	for x in args[1:]:
		x = eval(x, fun)
		sum -= x
	return sum
functions["-"] = symbol_minus


def symbol_one_minus(fun, args):
	return eval(args[0], fun) - 1
functions["1-"] = symbol_one_minus


def symbol_star(fun, args):
	result = eval(args[0], fun)
	for x in args[1:]:
		x = eval(x, fun)
		result *= x
	return result
functions["*"] = symbol_star


def symbol_slash(fun, args):
	result = float(eval(args[0], fun))
	for x in args[1:]:
		x = eval(x, fun)
		if x == 0:
			raise LispError("/: Division by zero")
		result /= x
	return result
functions["/"] = symbol_slash


def symbol_mod(fun, args):
	result = float(eval(args[0], fun))
	for x in args[1:]:
		x = eval(x, fun)
		if x == 0:
			raise LispError("MOD: Division by zero")
		result %= x
	return result
functions["MOD"] = symbol_mod


def symbol_expt(fun, args):
	return math.pow(eval(args[0], fun), eval(args[1], fun))
functions["EXPT"] = symbol_expt


def symbol_sqrt(fun, args):
	return math.sqrt(eval(args[0], fun))
functions["SQRT"] = symbol_sqrt


def symbol_sin(fun, args):
	return math.sin(eval(args[0], fun))
functions["SIN"] = symbol_sin


def symbol_cos(fun, args):
	return math.cos(eval(args[0], fun))
functions["COS"] = symbol_cos


def symbol_tan(fun, args):
	return math.tan(eval(args[0], fun))
functions["TAN"] = symbol_tan


def symbol_asin(fun, args):
	return math.asin(eval(args[0], fun))
functions["ASIN"] = symbol_asin


def symbol_acos(fun, args):
	return math.acos(eval(args[0], fun))
functions["ACOS"] = symbol_acos


def symbol_atan(fun, args):
	return math.atan(eval(args[0], fun))
functions["ATAN"] = symbol_atan