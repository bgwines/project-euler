
import pyprimes
import copy
import pdb 

UB = 1500

def concat_still_prime(p1, p2):
	return (pyprimes.isprime(int(str(p1) + str(p2)))
		and pyprimes.isprime(int(str(p2) + str(p1))))

def valid(ps):
	for i in xrange(len(ps)):
		for j in xrange(i, len(ps)):
			if ps[i] == ps[j]:
				continue
			if not concat_still_prime(ps[i], ps[j]):
				return False
	return True

############################################

def can_assign(prime_concat_set, p):
	for pp in prime_concat_set:
		if not concat_still_prime(p, pp):
			#print p, pp, ' cannot be concatenated.'
			return False
	return True

def assign_to_sets(prime_concat_sets, p, n):
	to_add = []
	for prime_concat_set in prime_concat_sets:
		if can_assign(prime_concat_set, p):
			copied_prime_concat_set = copy.deepcopy(prime_concat_set)
			copied_prime_concat_set.append(p)
			to_add.append(copied_prime_concat_set)

			if len(copied_prime_concat_set) == n:
				print 'works: ', copied_prime_concat_set
				print 'num_sets: ', len(prime_concat_sets)

	prime_concat_sets.append([p])
	for prime_concat_set in to_add:
		prime_concat_sets.append(prime_concat_set)
	return False

############################################

prime_concat_sets = []

count = 0

i = 0
for p in pyprimes.primes():
	i += 1
	if i % 100 == 0:
		print i, p
	if i == UB:
		break

	assign_to_sets(prime_concat_sets, p, 5)