import pdb

lfg_cache = dict()
def lfg(k):
	if k in lfg_cache:
		return lfg_cache[k]

	if 1 <= k and k <= 55:
		lfg_cache[k] = ((100003 - 200003*k + 300007*(k**3)) % 1000000) - 500000
	else:
		lfg_cache[k] = ((lfg(k-24) + lfg(k-55)) % 1000000) - 500000
	return lfg_cache[k]

def in_bounds(matrix, i, j):
	return (0 <= i and i < len(matrix)
		and 0 <= j and j < len(matrix[i]))

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

def init_matrix():
	matrix = []
	k = 1
	for i in xrange(0, 2000):
		row = []
		for j in xrange(0, 2000):
			row.append(lfg(k))
			k += 1
		matrix.append(row)
	return matrix

def max_contiguous_subsequence_sum(sequence):
	max_sums_ending_at_indices = []
	for (i, x) in enumerate(sequence):
		max_sum_ending_at_index = x
		if i > 0:
			max_sum_ending_at_index = max(x, max_sums_ending_at_indices[i-1] + x)
		max_sums_ending_at_indices.append(max_sum_ending_at_index)
	return max(max_sums_ending_at_indices)

matrix = init_matrix()

horizontals = matrix
verticals = [[matrix[j][i] for j in xrange(0, len(matrix))] for i in xrange(0, len(matrix[0]))]
ldiags = [ldiag_from(matrix, 0, j) for j in xrange(len(matrix[0]))] + [ldiag_from(matrix, i, len(matrix[0])-1) for i in xrange(len(matrix))]
rdiags = [rdiag_from(matrix, 0, j) for j in xrange(len(matrix[0]))] + [rdiag_from(matrix, i, 0) for i in xrange(len(matrix))]

print max(
	[ max(map(max_contiguous_subsequence_sum, horizontals))
	, max(map(max_contiguous_subsequence_sum, verticals))
	, max(map(max_contiguous_subsequence_sum, ldiags))
	, max(map(max_contiguous_subsequence_sum, rdiags)) ])
