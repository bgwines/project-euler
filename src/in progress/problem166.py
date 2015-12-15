
import pdb
import copy

def gen_seqs(so_far=[]):
	if len(so_far) == 4:
		return [tuple(so_far)]

	seqs = []
	for n in range(0, 9+1):
		so_far_copy = copy.copy(so_far)
		so_far_copy.append(n)
		seqs += gen_seqs(so_far_copy)
	return seqs

def prefix_seq_maps(seqs):
	sums_to_prefixes_to_seqs = []
	for s in range(0, 9*4+1):
		sums_to_prefixes_to_seqs.append(dict())

	for seq in seqs:
		s = sum(seq)
		for i in range(1, len(seq)+1):
			prefix = seq[0:i]
			if prefix not in sums_to_prefixes_to_seqs[s]:
				sums_to_prefixes_to_seqs[s][prefix] = []
			sums_to_prefixes_to_seqs[s][prefix].append(seq)
	return sums_to_prefixes_to_seqs

def vertical_prefixes(grid):
	return [
		tuple([grid[i][j] for i in range(len(grid))])
		for j in range(len(grid[0]))
	]

def diag_from(matrix, i, j, i_increment, j_increment):
	diag = []
	while in_bounds(matrix, i, j):
		diag.append(matrix[i][j])
		i += i_increment
		j += j_increment
	return diag

def ldiag_from(matrix, i, j):
	return diag_from(matrix, i, j, 1, -1)

def rdiag_from(matrix, i, j):
	return diag_from(matrix, i, j, 1, 1)

def diag_prefixes(grid):
	return [tuple(ldiag_from(grid, 0, 0)), tuple(rdiag_from(grid, 0, len(grid[0]-1)))

def num_fillings_with_sum(s, grid=[]):
	prefixes = []
	if grid != []:
		prefixes = vertical_prefixes(grid)# + diag_prefixes(grid)

	valid_digits = 

	return 1

seqs = gen_seqs()
seqset = set(seqs)
sums_to_prefixes_to_seqs = prefix_seq_maps(seqs)

num_fillings = 0
for s in range(0, 9*4+1):
	num_fillings += num_fillings_with_sum(s)
print num_fillings
