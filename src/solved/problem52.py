
import pdb
import pyprimes
import copy

def gen_powerset(src, so_far, powerset):
    if len(src) == 0:
        powerset.append(so_far)
        return

    e = src[0]
    src = src[1:]

    gen_powerset(src, copy.deepcopy(so_far), powerset)

    so_far.append(e)
    gen_powerset(src, so_far, powerset)

def gen_powerset_wrapper(src):
	powerset = []
	gen_powerset(src, [], powerset)
	return powerset

def repl(s, i, ch):
	return s[0:i] + ch + s[i+1:]

def replace_digits(s, indices, ch):
	for index in indices:
		s = repl(s, index, ch)
	return s

def get_prime_repl_count_print(p, index_subset):
	if index_subset == []:
		if pyprimes.isprime(int(p)):
			return 1
		else:
			return 0

	count = 0
	for replacement_digit in ['0','1','2','3','4','5','6','7','8','9']:
		p_replaced = int(replace_digits(str(p), index_subset, replacement_digit))
		if pyprimes.isprime(p_replaced) and (len(str(p_replaced)) == len(str(p))):
			print p_replaced
			count += 1
	return count

def get_prime_repl_count(p, index_subset):
	if index_subset == []:
		if pyprimes.isprime(int(p)):
			return 1
		else:
			return 0

	count = 0
	for replacement_digit in ['0','1','2','3','4','5','6','7','8','9']:
		p_replaced = int(replace_digits(str(p), index_subset, replacement_digit))
		if pyprimes.isprime(p_replaced) and (len(str(p_replaced)) == len(str(p))):
			count += 1
	return count

def part_of_n_prime_family(p, n):
	indices = [i for i in xrange(len(str(p)))]
	index_subsets = gen_powerset_wrapper(indices)
	
	best_count = 0
	best_subset = []
	for index_subset in index_subsets:
		count = get_prime_repl_count(p, index_subset)
		if count > best_count:
			best_count = count
			best_subset = index_subset

	if best_count == n:
		print p, best_subset
		return True
	else:
		return False

i = 0
pdb.set_trace()
for p in pyprimes.primes():
	i += 1
	if i % 1000 == 0:
		print '#', i, ' - ', p
	if part_of_n_prime_family(str(p), 8):
		break