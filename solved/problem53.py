
def fact(n):
	prod = 1
	while n != 0:
		prod *= n
		n -= 1
	return prod 

def C(n,r):
	return fact(n) / (fact(r) * fact(n-r))

count = 0
for n in xrange(1, 100+1):
	for r in xrange(0, n):
		if C(n, r) > 1000000:
			count += 1

print count