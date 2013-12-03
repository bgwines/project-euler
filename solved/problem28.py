SIDE_LEN = 1001

odds = [n for n in xrange(1, 1 + SIDE_LEN, 2)]
evens = [n for n in xrange(2, 1 + SIDE_LEN, 2)]
odd_squares = map(lambda n: n**2, odds)
even_squares = map(lambda n: n**2, evens)

UL = map(lambda n: n + 1, even_squares)
UR = map(lambda n: int(n - (n**0.5) + 1), even_squares)
DL = map(lambda n: int(n - (n**0.5) + 1), odd_squares[1:])
DR = odd_squares

diags = UL + UR + DL + DR

print diags, sum(diags)