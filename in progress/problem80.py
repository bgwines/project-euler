
import itertools
import pdb
import sys

def calc_num_rects(n, m):
	return (m) * (m+1) * (n) * (n+1) / 4

curr_best = -1
for n in xrange(1,100):
	for m in xrange(1,n):
		num_rects = calc_num_rects(n, m)
		if (abs(num_rects - 2000000) < abs(curr_best - 2000000)) or (curr_best == -1):
			curr_best = num_rects
			print n, m, num_rects, n*m