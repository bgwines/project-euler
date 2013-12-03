
import pdb

def collatz(n):
	if n % 2 == 0:
		return n / 2
	else:
		return (3 * n) + 1

def calc_collatz_seq_len(n, lengths):
	if n == 1:
		return 1
	if n in lengths:
		return lengths[n]
	
	lengths[n] = 1 + calc_collatz_seq_len(collatz(n), lengths)
	return lengths[n]

lengths = {}

max_so_far = -1
max_so_far_elem = None
for n in xrange(2, 1000000):
	calc_collatz_seq_len(n, lengths)
	if lengths[n] > max_so_far:
		max_so_far = lengths[n]
		max_so_far_elem = n

print max_so_far_elem