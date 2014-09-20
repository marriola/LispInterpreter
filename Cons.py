import Atom


class Cons:
	def __init__(self, first, second, parent=None):
		"""Constructs a cons with first and second being objects of type Atom"""
		self.first = first
		self.second = second
		self.parent = parent
		self.quote = 0

	def __str__(self):
		# if our parent is an atom, grab the cons out of it -- that's the real parent
		parent = self.parent
		if parent is Atom.Atom:
			parent = parent.data

		# check if we are part of a list (in which case our parent is a cons and our cdr points to another cons)
		if self.second.type == Atom.Atom.CONS or (parent and parent.type == Atom.Atom.CONS):
			# show opening paren if this is the root of a list
			str = "(" if not self.parent or self.first.type == Atom.Atom.CONS else ""

			if self.second == Atom.NIL:
				str += "%s" % self.first
			else:
				str += "%s %s" % (self.first, self.second)

			if self.second == Atom.NIL:
				str += ")"

		else:
			str = "(%s . %s)" % (self.first, self.second)

		return str


def orphan(cons):
	"""Creates a list with no parent from a list. All atoms are kept in place, but a new root cons object with no parent
	is created to contain the first atom of the list."""

	new_atom = Atom.build_atom_from_cons(cons.data.first, cons.data.second)
	new_atom.quote = cons.quote
	return new_atom