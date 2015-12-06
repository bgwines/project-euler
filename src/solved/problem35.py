
import pyprimes
import pdb

def cycle(l):
	if type(l) == type(''):
		return l[1:] + l[0]
	else:
		return l[1:] + [l[0]]

def gen_cycles(l):
	ll = cycle(l)
	cycles = [l]
	while (ll != l):
		cycles.append(ll)
		ll = cycle(ll)
	return cycles

def is_circular_prime(p):
	l = map(pyprimes.isprime, map(int, (gen_cycles(str(p)))))
	for b in l:
		if not b:
			return False
	return True

def filter(P, l):
    ll = []
    for e in l:
        if P(e):
                ll.append(e)
    return ll

UB = 1000000

primes_under_ub = []
for p in pyprimes.primes():
	if p > UB:
		break
	primes_under_ub.append(p)

l = filter(is_circular_prime, primes_under_ub)
sought = len(l)