
import pdb
import pyprimes
import copy

def gen_powerset_rec(src, so_far, powerset):
	if len(src) == 0:
		powerset.append(so_far)
		return

	e = src[0]
	src = src[1:]

	gen_powerset_rec(src, copy.deepcopy(so_far), powerset)

	so_far.append(e)
	gen_powerset_rec(src, so_far, powerset)

def gen_powerset(src):
	powerset = []
	gen_powerset_rec(src, [], powerset)
	return powerset

def flatten_1(l):
	ll = []
	for e in l:
		for ee in e:
			ll.append(ee)
	return ll

def get_products(l):
	prods = [1]
	for ns in l:
		new_prods = map(lambda x: [x*n for n in ns], prods)
		new_prods = flatten_1(new_prods)
		for new_prod in new_prods:
			prods.append(new_prod)

	return prods

def filter(P, l):
	pdb.set_trace()
	ll = []
	lenl = len(l)
	for i, e in enumerate(l):
			if i % 1000 == 0:
					print '(1) ', i, '/', lenl
			if P(e):
					ll.append(e)
	return ll

def factorize(n):
	prime_powers = []
	for p, a in pyprimes.factorise(n):
		l = []
		for expnt in xrange(1, a+1):
			l.append(p ** expnt)
		prime_powers.append(l)

	powerset = gen_powerset(prime_powers)

	prods = map(get_products, powerset)
	s = set()
	for prod_list in prods:
		for prod in prod_list:
			s.add(prod)
	s.remove(n)
	return list(s)

def get_sums_of_two_abundants(abundants):
	sums = set()
	for i, a in enumerate(abundants):
		for b in abundants[i:]:		
			sums.add(a+b)
	return sums

nums = [n for n in xrange(1, 28123+1)]

abundants = filter((lambda n: n < (sum (factorize(n)))), nums)

pdb.set_trace()
abundant_sums = get_sums_of_two_abundants(abundants)

not_sum_of_two_abundants = filter(lambda n: n not in abundant_sums, nums)

print not_sum_of_two_abundants
print sum(not_sum_of_two_abundants)
