
import copy
import itertools
import pdb
import pyprimes

def get_next_partitions(l, e):
	for i, _ in enumerate(l):
		l_copy = copy.deepcopy(l)
		l_copy[i].append(e)
		yield l_copy

	l_copy = copy.deepcopy(l)
	l_copy.append([e])
	yield l_copy

# powerpartition :: [a] -> [partition], where partition :: [[a]]
def powerpartition(l):
	if len(l) == 0:
		yield []
		raise StopIteration
	if len(l) == 1:
		yield [[l[0]]]
		raise StopIteration

	for partition in powerpartition(l[1:]):
		for next_partition in get_next_partitions(partition, l[0]):
			yield next_partition

def product(l):
	prod = 1
	for e in l:
		prod *= e
	return prod

def get_set_sizes(n):
	set_sizes = []
	for partition in powerpartition(pyprimes.factors(n)):
		s = sum(map(product, partition))
		set_size = n - s + len(partition)
		if set_size != 1: # i.e. prod([n]) == sum([n])
			set_sizes.append(set_size)
	return set_sizes

printed = set()
UB = 12000
n_ks_gotten = 0
ks = [-1 for _ in xrange(0, UB+1)]

for n in itertools.count(4):
	if n % 25 == 0:
		if n_ks_gotten not in printed:
			printed.add(n_ks_gotten)
			print n_ks_gotten, '/', UB, ' - ', n

	if n_ks_gotten == UB-1:
		break

	ks_gotten = get_set_sizes(n)
	for k in ks_gotten:
		if k >= len(ks):
			continue
		if (ks[k] == -1) or (ks[k] > n):
			n_ks_gotten += 1
			ks[k] = n

print sum(set(ks[2:]))