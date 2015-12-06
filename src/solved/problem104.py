
import pdb

UB = 10**6

fib_ends = [(0, 0) for x in xrange(0, UB)]
def fill_fib_ends():
	fib_ends[100] = (35422484817926191507, 261915075)
	fib_ends[101] = (57314784401381708410, 817084101)

	for n in xrange(102, UB):
		n1 = str(fib_ends[n-1][0])
		n2 = str(fib_ends[n-2][0])
		if (len(n1) == 21) and (len(n2) == 21):
			fib_ends[n-1] = (int(str(fib_ends[n-1][0])[0:20]), fib_ends[n-1][1])
			fib_ends[n-2] = (int(str(fib_ends[n-2][0])[0:20]), fib_ends[n-2][1])

		(a1, b1) = fib_ends[n-1]
		(a2, b2) = fib_ends[n-2]

		a = int(str(a1 + a2))
		
		b12 = str(b1 + b2)
		if (len(b12) > 9):
			b12 = b12[(len(b12) - 9):]
		b = int(b12)

		fib_ends[n] = (a, b)

def pandigital(s):
	return sorted(str(s)[0:9]) == ['1', '2', '3', '4', '5', '6', '7', '8', '9']

def both_ends_pandigital((a, b)):
	return pandigital(a) and pandigital(b)

def either_end_pandigital((a, b)):
	return pandigital(a) or pandigital(b)

fill_fib_ends();

for n in xrange(100, UB):
	if both_ends_pandigital(fib_ends[n]):
		print n
		break
