
import pdb
import pyprimes

n = 4

def have_intersection(l1, l2):
	return len(set(l1).union(set(l2))) != len(l1) + len(l2)

def get_factors(consec_ints):
	return map(
		lambda gen: [e for e in gen], 
		map(
			pyprimes.factorise, 
			consec_ints
		)
	)

def not_all_have_n_factors(factors):
	for l in factors:
		if len(l) != n:
			return True
	return False

# at index i, whether shares no factors with all i+1 in consec_ints
def calc_distinct_factorness(consec_ints):
	factors = get_factors(consec_ints)
	if not_all_have_n_factors(factors): #TODO: optimize?
		return None

	distinct_factorness = [None for i in consec_ints]
	for i in range(n-1, -1, -1):
		intersection = False
		for j in range(i+1, n):
			if have_intersection(factors[i], factors[j]):
				intersection = True
				break
		distinct_factorness[i] = not intersection
	return distinct_factorness

def get_increment(distinct_factorness):
	for i in range(n-1, -1, -1):
		if not distinct_factorness[i]:
			return i
	return -1

consec_int_lists = [[i, i+1, i+2, i+3] for i in xrange(700000)]
for i in xrange(len(consec_int_lists)):
	if i % 10000 == 0:
		print consec_int_lists[i][0], '/', 700000

	distinct_factorness = calc_distinct_factorness(consec_int_lists[i])
	if distinct_factorness == None:
		continue

	increment = get_increment(distinct_factorness)

	if increment == -1: # all factors distinct
		print consec_int_lists[i]
		break
	else:
		if increment == 0:
			i += 1
		else:
			i += increment