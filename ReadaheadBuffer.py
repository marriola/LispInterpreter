class ReadaheadBuffer:
	def __init__(self, stream):
		self._stream = stream
		self._buffer = ""

	def peek(self, num):
		while len(self._buffer) < num:
			self._buffer += self._stream.read(1)
		return self._buffer

	def read(self, num):
		out = ""
		for i in range(num):
			if self._buffer:
				out += self._buffer[0]
				self._buffer = self._buffer[1:]
			else:
				out += self._stream.read(1)
		return out

	def putback(self, ch):
		self._buffer = ch + self._buffer