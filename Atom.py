from __future__ import print_function
import io
from collections import defaultdict
import Cons
from Functions import Functions
import Environment
from LispError import LispError
import LispParser
from ReadaheadBuffer import ReadaheadBuffer
import re


class Atom:
	BOOLEAN, SYMBOL, CONS, INTEGER, DECIMAL, CHARACTER, STRING, QUOTE, KEYWORD, FUNCTION, VECTOR = range(11)

	def __init__(self, type, data, key=None, quote=0):
		self.type = type
		self.data = data.upper() if type == self.SYMBOL else data
		self.key = key
		self.quote = quote

	def __str__(self):
		str = "'" * self.quote

		if self.type == self.BOOLEAN:
			str += "T" if self.data else "NIL"

		elif self.type == self.CONS:
			str += self.data.__str__()

		elif self.type == self.SYMBOL:
			str += self.data

		elif self.type == self.INTEGER:
			str += "%d" % self.data

		elif self.type == self.DECIMAL:
			str += "%f" % self.data

		elif self.type == self.CHARACTER:
			str += "#\%s" % self.data

		elif self.type == self.STRING:
			str += "\"%s\"" % self.data

		elif self.type == self.KEYWORD:
			str += ":" + self.data

		elif self.type == self.QUOTE:
			str += "'%s" % self.data

		elif self.type == self.FUNCTION:
			str += "#%s" % self.data

		elif self.type == self.VECTOR:
			str += "#("
			for i in range(len(self.data)):
				x = self.data[i]
				str += x.__str__()
				if i < len(self.data) - 1:
					str += " "
				if i == 20:
					str += "... (%d more)" % (len(self.data) - 20)
					break
			str += ")"

		else:
			str += "<unknown>"

		return str

	def clone(self):
			new_atom = Atom(self.type, self.data, self.quote)
			return new_atom


def build_atom_from_cons(car, cdr, parent=None):
	"""Build an atom containing a Cons object

	Keyword arguments:
	car		-- the first element of the cons
	cdr		-- the second element of the cons
	parent	-- the parent cons of this cons"""

	return Atom(Atom.CONS, Cons.Cons(car, cdr, parent))


def atom_from_cons(cons):
	"""Creates an atom containing a given cons object"""
	return Atom(Atom.CONS, cons)


def make_atom(data):
	"""Encapsulates data in an Atom object"""
	t = type(data)

	if t is bool:
		t = Atom.BOOLEAN
	elif t is int or t is long:
		t = Atom.INTEGER
	elif t is float:
		t = Atom.DECIMAL
	elif t is str:
		if data[0] == "\"" and data[-1] == "\"":
			t = Atom.STRING
			data = data[1:-1]
		elif data[0:2] == "#\\":
			t = Atom.CHARACTER
			data = data[2]
		elif data[0] == "#":
			t = Atom.FUNCTION
			data = data[1:].upper()
		elif data[0] == ":":
			t = Atom.KEYWORD
			data = data[1:]

	else:
		t = Atom.SYMBOL

	return Atom(t, data)


def make_symbol(data):
	"""Creates an atom containing a symbol"""
	return Atom(Atom.SYMBOL, data.upper())


def make_quote(data):
	"""Returns a quote of an atom"""
	return Atom(Atom.QUOTE, data)


NIL = make_atom(False)
T = make_atom(True)


def add_to_list(cons, item):
	"""Adds an item to a list

	:param cons: A cons pointing to the last item of the list to add to
	:param item: The item to append to the end of the list
	:return:	 A cons object containing the newly added item
	"""

	if type(item) is list:
		cons.second = build_atom_from_cons(atom_from_cons(make_list(item)), NIL, cons)
	else:
		cons.second = build_atom_from_cons(make_atom(item), NIL, cons)
	return cons.second.data


def make_list(list):
	root = cons = Cons.Cons(make_atom(list[0]), NIL)
	list = list[1:]

	for x in list:
		cons = add_to_list(cons, x)

	return root


def make_vector(v, fill=None):
	if type(v) is int:
		list = []
		for i in range(v):
			list += [fill.clone()]
		return Atom(Atom.VECTOR, list)
	else:
		return Atom(Atom.VECTOR, v)


def build_vector_from_string(str):
	wrapper = io.TextIOWrapper(io.BytesIO(str + "\n"))
	buf = ReadaheadBuffer(wrapper)
	vec_list = []
	vec_parser = LispParser.LispParser(buf)
	while True:
		try:
			vec_list += [vec_parser.read_object()]
		except StopIteration:
			break
		peek = buf.read(1)
		if peek == ")":
			break
		else:
			buf.putback(peek)
	new_vector = make_vector(vec_list)
	new_vector.quote += 1
	return new_vector


def list_item(list):
	return list.first.data


def index_into_list(list, position):
	if position == 0:
		return list.first
	else:
		return index_into_list(list.second.data, position - 1)


def index_from_list(list, position):
	if position == 0:
		return list
	else:
		return index_from_list(list.second.data, position - 1)


def advance_list(list):
	"""if list.second.type == Atom.QUOTE:
		return build_atom_from_cons(list.second, list.second.data.second)
	else:"""
	return list.second.data


def cons_to_list(cons):
	"""Converts a Lisp cons object to a Python list

	:rtype : Atom
	"""
	result = []
	while cons:
		if cons.first and cons.first.type == Atom.CONS:
			item = Cons.orphan(cons.first)
			if cons.quote:
				item.quote = cons.quote
		else:
			item = cons.first
		result.append(item)
		cons = cons.second.data
	return result


def evaluate(atom, called_from="", return_object=False):
	if atom.quote:
		atom.quote -= 1
		return atom

	elif atom.type == Atom.KEYWORD:
		return atom.data

	elif atom.type == Atom.SYMBOL:
		value = Environment.get_symbol_value(atom.data)
		if value is not None:
			return value
		else:
			raise LispError("%s: Undefined symbol %s" % (called_from, atom.data))

	elif atom.type == Atom.CONS:
		if atom.data.quote:
			atom.data.quote -= 1
			return atom

		fun = atom.data.first.data
		if fun not in Functions.functions:
			raise LispError("%s: Undefined function %s" % (called_from, fun))

		args = cons_to_list(atom.data.second.data)
		result = Functions.functions[fun](fun, args)
		return result

	else:
		return atom if return_object else atom.data


def detect_type(token):
	"""Detects a token's type and returns it encapsulated within an Atom."""

	# quote
	if token[0] == "'":
		return make_quote(detect_type(token[1:]))

	# function, keyword, string or character
	elif token[0] in ("#", ":") or re.match("\".*\"$", token) or re.match("\\#\\\\.$", token):
		return make_atom(token)

	elif re.match("\\#\\(.*\\)", token):
		return build_vector_from_string(token[2:-1])

	elif token.upper() == "T":
		return T

	elif token.upper() == "NIL":
		return NIL

	elif re.match("-?[0-9]+$", token) or re.match("#[xX][0-9a-fA-F]+$", token):
		return make_atom(int(token))

	elif re.match("-?[0-9]*\.[0-9]+$", token):
		return make_atom(float(token))

	# otherwise assume symbol
	else:
		return make_symbol(token)