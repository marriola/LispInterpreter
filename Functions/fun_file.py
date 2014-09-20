import sys
import Atom
from LispError import LispError
import LispParser
from Functions import eval, functions


def symbol_load(fun, args):
	filename = eval(args[0], fun)
	print("Loading %s" % filename)
	try:
		file = open(filename)
	except Exception as e:
		print("Failed (%s)" % e)
		return Atom.NIL

	parser = LispParser.LispParser(file)
	while True:
		try:
			atom = parser.read_object()
		except StopIteration:
			break

		try:
			result = Atom.evaluate(atom, fun, True)
		except LispError as err:
			print("Error: %s" % err)
	file.close()
	print("Done")
	return Atom.NIL
functions["LOAD"] = symbol_load

file_handles = {0: sys.stdin, 1: sys.stdout, 2: sys.stderr}
next_handle = 3


def symbol_open(fun, args):
	global file_handles, next_handle
	filename = eval(args[0], fun)
	try:
		file_handles[next_handle] = open(filename)
		next_handle += 1
		return next_handle - 1
	except Exception as e:
		raise LispError("OPEN: Couldn't open '%s' (%s)" % (filename, e.__str__()))
functions["OPEN"] = symbol_open


def symbol_read_char(fun, args):
	handle = eval(args[0], fun)
	if handle not in file_handles:
		raise LispError("READ-CHAR: File handle not active")
	ch = file_handles[handle].read(1)
	if not ch:
		return Atom.make_symbol("EOF")
	return "\"%s\"" % ch
functions["READ-CHAR"] = symbol_read_char


def symbol_read(fun, args):
	handle = eval(args[0], fun)
	if handle not in file_handles:
		raise LispError("READ: File handle not active")
	try:
		parser = LispParser.LispParser(file_handles[handle])
		return parser.read_object()
	except StopIteration:
		return Atom.make_symbol("EOF")
functions["READ"] = symbol_read


def symbol_read_line(fun, args):
	handle = eval(args[0], fun)
	if handle not in file_handles:
		raise LispError("READ-LINE: File handle not active")
	try:
		while True:
			line = file_handles[handle].readline()
			if not line:
				raise LispError("READ-LINE: End of file")
			if line != "\n":
				break

		if line[-1] == "\n":
			line = line[:-1]
		return Atom.Atom(Atom.Atom.STRING, line)
	except Exception as e:
		raise LispError(e.__str__())
functions["READ-LINE"] = symbol_read_line


def symbol_close(fun, args):
	handle = eval(args[0], fun)
	if handle not in file_handles:
		raise LispError("CLOSE: File handle not active")
	file_handles[handle].close()
	file_handles.pop(handle)
functions["CLOSE"] = symbol_close