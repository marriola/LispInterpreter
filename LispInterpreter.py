from __future__ import print_function
import sys
import Atom
from LispError import LispError
import LispParser
from ReadaheadBuffer import ReadaheadBuffer

if len(sys.argv) > 1:
	file = open(sys.argv[1])
	show_prompt = False
else:
	file = ReadaheadBuffer(sys.stdin)
	show_prompt = True

parser = LispParser.LispParser(file)

while True:
	if show_prompt:
		print("> ", end="")
	try:
		atom = parser.read_object()
	except StopIteration:
		break
	except LispError as err:
		print("Parser error: %s\n" % err)
		continue
	except KeyboardInterrupt:
		print("<<keyboard interrupt>>")
		continue

	try:
		result = Atom.evaluate(atom, True)
		if show_prompt:
			#if result != None:
			print(result)
			print()
	except LispError as err:
		print("Error: %s\n" % err)
	except KeyboardInterrupt:
		print("<<keyboard interrupt>>")
	#except Exception as err:
	#	print("Python exception: %s" % err)