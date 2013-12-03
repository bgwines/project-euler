
import pdb
import pyprimes

def filter(P, l):
	ll = []
	lenl = len(l)
	for i, e in enumerate(l):
		if P(e):
				ll.append(e)
	return ll

SIDE_LEN = 27699

odds = [n for n in xrange(1, 1 + SIDE_LEN, 2)]
evens = [n for n in xrange(2, 1 + SIDE_LEN, 2)]
odd_squares = map(lambda n: n**2, odds)
even_squares = map(lambda n: n**2, evens)

UL = map(lambda n: n + 1, even_squares)
UR = map(lambda n: int(n - (n**0.5) + 1), even_squares)
DL = map(lambda n: int(n - (n**0.5) + 1), odd_squares[1:])
DR = odd_squares

diags = []
for i in xrange(len(UL)):
	diags.append(UL[i])
	diags.append(UR[i])
	diags.append(DL[i])
	diags.append(DR[i])

diags.append(DR[len(UL)])

ndiagprimes = len(filter(pyprimes.isprime, diags))

nlostprimes = 0
while (SIDE_LEN > 0):
	ratio = 1.0 * ndiagprimes / len(diags)
	if ratio < 0.1:
		print SIDE_LEN, ratio

	SIDE_LEN -= 2

	nlostprimes = len(filter(pyprimes.isprime, diags[len(diags) - 4:]))
	ndiagprimes -= nlostprimes

	diags = diags[0 : len(diags) - 4]


