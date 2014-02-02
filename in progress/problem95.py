
import euler
import itertools
import pdb

divisors = {}
chain_lengths = {}

def increment(n):
	if n not in divisors:
		divisors[n] = tuple(euler.get_proper_divisors(n))
	return sum(divisors[n])

def calc_chain_length_rec(starting_n, curr_n):
	if curr_n in chain_lengths:
		return chain_lengths[curr_n]

	if curr_n > 1000000:
		return -1

	if curr_n == starting_n:
		return 0

	next_n = increment(curr_n)

	rec_val = calc_chain_length_rec(starting_n, next_n)
	chain_lengths[next_n] = rec_val

	if rec_val == -1:
		return -1
	else:
		return 1 + rec_val

def calc_chain(n):
	chain = [n]
	curr_n = increment(n)
	while curr_n != n:
		chain.append(curr_n)
		curr_n = increment(curr_n)
	return chain

def calc_chain_min_value(n):
	return euler.minimum(calc_chain(n))

def calc_chain_length(n):
	if n not in divisors:
		divisors[n] = euler.get_proper_divisors(n)
	curr_n_divisors = divisors[n]
	
	ret_val = 1 + calc_chain_length_rec(n, sum(curr_n_divisors))
	chain_lengths[n] = ret_val
	return ret_val

pdb.set_trace()
value_with_max_len = -1
for i in xrange(1,300):
	chain_length = calc_chain_length(i)
	if (value_with_max_len == -1) or (chain_length < chain_lengths[value_with_max_len]):
		value_with_max_len = i
		print i, calc_chain(i), calc_chain_min_value(i)
