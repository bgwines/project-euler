import copy
import pdb

INDICES = [0,1,2,3,4,5,6,7,8]
DIGITS = [1,2,3,4,5,6,7,8,9]

def printgrid(grid):
	print '-------------------------'
	i = 0
	for row in grid:
		if i % 3 == 0 and i != 0:
			print '-------------------------'
		j = 0
		print '|',
		for e in row:
			if j % 3 == 0 and j != 0:
				print '|',
			print e,
			j += 1
		print '|'
		i += 1
	print '-------------------------'
	print ''
	print ''

def get_square_indices(i):
	if   i < 3: return [0,1,2]
	elif i < 6: return [3,4,5]
	elif i < 9: return [6,7,8]

def get_square(grid, i, j):
	rows = get_square_indices(i)
	cols = get_square_indices(j)
	return [grid[x][y] for y in cols for x in rows]

def get_valid_digits(grid, i, j):
	row = set(grid[i])
	col = set([grid[k][j] for k in INDICES])
	square = get_square(grid, i, j)
	valid_digits = []
	for digit in DIGITS:
		if (digit not in row and
			digit not in col and
			digit not in square
		):
			valid_digits.append(digit)
	return valid_digits

def get_zero_indices(grid):
	placements = []
	for i, row in enumerate(grid):
		for j, _ in enumerate(row):
			if grid[i][j] != 0:
				continue
			placements.append((i, j))

	placements.sort(key=lambda(i, j): len(get_valid_digits(grid, i, j)))
	return placements

def can_solve_grid_rec(grid, zero_indices):
	if len(zero_indices) == 0:
		return grid

	(i, j) = zero_indices[0]
	for d in get_valid_digits(grid, i, j):
		grid[i][j] = d
		if can_solve_grid_rec(grid, zero_indices[1:]):
			return grid
		grid[i][j] = 0
	return None

def can_solve_grid(grid):
	zero_indices = get_zero_indices(grid)
	return can_solve_grid_rec(grid, zero_indices)

def make_row(line):
	row = []
	for ch in line.strip():
		row.append(int(ch))
	return row

def get_initial_grids():
	grids = []
	curr_grid = []
	for line in open('grids.txt', 'r'):
		if line == '\n':
			grids.append(curr_grid)
			curr_grid = []
			continue

		curr_grid.append(make_row(line))
	grids.append(curr_grid)
	return grids

def calc_n_remaining(grid):
	n_remaining = 0
	for row in grid:
		for e in row:
			if e == 0:
				n_remaining += 1
	return n_remaining

i = 1
the_sum = 0
for initial_grid in get_initial_grids():
	print i, '/ 50: ',
	solution = can_solve_grid(initial_grid)
	if solution:
		upper_left_strcat = int(''.join([str(digit) for digit in solution[0][0:3]]))
		the_sum += upper_left_strcat
		print upper_left_strcat
		printgrid(solution)
	else:
		print ' no solution for this grid: '
		print initial_grid
		break
	i += 1

print the_sum