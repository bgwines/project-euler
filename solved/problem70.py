
import copy
import pyprimes
import pdb

def fast_euler_phi(n, totients):
	if n == 2:
		return 1
	if n % 2 == 0:
		if (n / 2) % 2 == 0:
			return 2 * totients[n / 2]
		else:
			return 1 * totients[n / 2]
	else:
		if pyprimes.isprime(n):
			return n-1
		else:
			phi = 1
			for (p, a) in pyprimes.factorise(n):
				phi *= (p ** (a-1)) * (p-1)
			return phi

def get_elem_counts(s):
    d = {}
    for ch in str(s):
            if ch in d:
                    d[ch] += 1
            else:
                    d[ch] = 1
    return (tuple(d.keys()), tuple(d.values()))

def is_permutation(a, b):
	return get_elem_counts(str(a)) == get_elem_counts(str(b))

UB = 10**7
SENTINEL = -1
min_ratio = SENTINEL
best_n = -1
totients = [0, 0]
i = 0
for n in xrange(1, UB):
	i += 1
	if i % 10000 == 0:
		print (i*1.0) / 10**7, '%'

	phi = fast_euler_phi(n, totients)
	totients.append(phi)

	if is_permutation(phi, n):
		ratio = (1.0 * n) / (1.0 * phi)
		if ratio < min_ratio or min_ratio == SENTINEL:
			print phi, n, ' are permutations of each other. The ratio is ', ratio
			print ' which is better than ', min_ratio
			min_ratio = ratio
			best_n = n
