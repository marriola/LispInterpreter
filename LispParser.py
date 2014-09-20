import Atom
import sys
import io
from LispError import LispError


class LispParser:
	"""
	Reads an object from standard input and returns it.	Accepts both s-expressions and m-expressions.

	Parser state variables

	root				An Atom containing a Cons pointing to the first item of the resulting list
	this_atom			An Atom containing a Cons pointing to the next item in the list
	place_first_atom	The first atom to place in a new m-expression

	level				The current list depth level
	quote_level			The level at which the most recent quoted object was quoted
	token				The token being read in. May be unfinished.
	finish_token		A finished token just read in.

	in_quotation		The parser is currently in an open quotation
	in_comment			The parses is currently in an open comment
	open_paren			The parser just encountered an open parenthesis
	close_paren			The parser just encountered a closed parenthesis
	last				The last character read by the parser (initially None)
	"""

	def __init__(self, input=sys.stdin):
		self._input = input
		self._root = None
		self._this_atom = None
		self._place_first_atom = None

		self._level = 0
		self._quote_level = None
		self._quote_atom = None
		self._token = ""
		self._finish_token = ""

		self._in_character_quote = False
		self._in_quotation = False
		self._in_escape = False
		self._in_comment = False
		self._open_paren = False
		self._close_paren = False
		self._last = None

	def _open_sublist(self):
		"""Appends an empty sublist to the current list. After completion, _this_atom will point to the sublist."""

		# opening a sublist at top level
		if self._level == 1:
			next_atom = Atom.build_atom_from_cons(self._place_first_atom, Atom.NIL, self._this_atom)
			if self._this_atom:
				self._this_atom.data.second = next_atom

		# adding sublist below top level
		else:
			sublist = Atom.build_atom_from_cons(None, Atom.NIL, self._this_atom)
			if self._this_atom:
				if self._this_atom.data.first:
					self._this_atom.data.second = sublist
				else:
					self._this_atom.data.first = sublist

				# if the new sublist had to go into the second field, then we don't need to stick the next atom
				# inside another sublist
				if not self._this_atom.data.second == sublist:
					sublist.data.first = self._place_first_atom
					next_atom = sublist
				else:
					next_atom = Atom.build_atom_from_cons(self._place_first_atom, Atom.NIL, sublist)
					sublist.data.first = next_atom

		if not self._root:
			self._root = next_atom
		self._this_atom = next_atom

	def _close_sublist(self):
		if self._this_atom:
			self._this_atom = self._this_atom.data.parent
		if self._level >= 2:
			# closing a sublist - unwind the list back to its root
			while self._this_atom and self._this_atom.data.first is not None and self._this_atom.data.second != Atom.NIL:
				self._this_atom = self._this_atom.data.parent

		self._level -= 1
		if self._level == self._quote_level:
			self._quote_level = None
			node = self._this_atom
			while node.data.parent and node.data.first.type != Atom.Atom.CONS:
				node = node.data.parent
			node.quote += 1
			if node.type == Atom.Atom.CONS:
				node.data.quote += 1

	def _process_character(self, ch):
		if self._in_comment:
			if ch == "\n":
				# end comment
				self._in_comment = False

		elif self._in_quotation or self._in_character_quote:
			if self._in_escape:
				self._in_escape = False
				if ch == "\\":
					self._token += "\\"
				elif ch == "\r":
					self._token += "\r"
				elif ch == "\n":
					self._token += "\n"
				elif ch == "\t":
					self._token += "\t"
				elif ch == "\"":
					self._token += "\""
				else:
					raise LispError("Unknown escape sequence \\%s" % ch)

			elif ch == "\\":
				self._in_escape = True
				return

			else:
				self._token += ch
				if ch == "\"" and not self._in_character_quote:
					self._in_quotation = False
					self._finish_token = self._token
					self._token = ""
				self._in_character_quote = False

		elif ch == ";":
			# begin comment
			self._in_comment = True

		elif ch == "'":
			# quoted object begins here
			self._quote_level = self._level
			#self._quote_atom = self._this_atom

		elif ch.isspace() or ch == ",":
			# token delimiter
			if self._last and not self._last.isspace():
				self._finish_token = self._token
				self._token = ""

		elif ch == "(":
			if self._last == "#":
				self._token = "#("
				while True:
					peek = self._input.read(1)
					self._token += peek
					if peek == ")":
						break
				self._finish_token = self._token
				self._token = ""
			else:
				# open s-expression
				self._level += 1
				self._open_paren = True

		elif ch == "[":
			# open m-expression
			self._place_first_atom = Atom.detect_type(self._token)
			self._token = ""
			self._level += 1
			self._open_paren = True

		elif ch == ")" or ch == "]":
			if self._level:
				# close s- or m-expression
				self._close_paren = True
				self._finish_token = self._token
				self._token = ""
			else:
				self._finish_token = self._token
				self._token = ""
				self._input.putback(")")

		elif ch == "\"":
			# open/close string
			self._token += ch
			self._in_quotation = True

		else:
			# add to token
			self._token += ch
			if self._token == "#\\":
				self._in_character_quote = True

		self._last = ch

	def _process_token(self):
		if self._finish_token:
			# just completed token
			# encapsulate the token inside an atom of the proper type
			next_atom = Atom.detect_type(self._finish_token)
			self._finish_token = ""

			# quote closed list
			if self._level == self._quote_level:
				self._quote_level = None
				#next_atom = Atom.make_quote(next_atom)
				next_atom.quote += 1

			# return completed atom outside of list
			if self._level == 0:
				return next_atom

			# append atom to current list
			if self._this_atom.data.first:
				# if we've already filled the current cons, place the current atom in a new cons and link to that
				next_cons = Atom.build_atom_from_cons(next_atom, Atom.NIL, self._this_atom)
				self._this_atom.data.second = next_cons
				self._this_atom = next_cons
			else:
				# otherwise just place it into the current cons
				self._this_atom.data.first = next_atom

		if self._open_paren:
			self._open_sublist()
			self._place_first_atom = None
			self._open_paren = False

		if self._close_paren:
			self._close_paren = False
			self._close_sublist()
			if self._level == 0:
				return self._root

	def read_object(self):
		result = None
		while True:
			ch = self._input.read(1)
			if not ch:
				# end of file
				raise StopIteration

			self._process_character(ch)
			result = self._process_token()
			if result:
				self.__init__(self._input)
				return result