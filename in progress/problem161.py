
import pdb
import copy

ANGLE_0 = 0 # |_
ANGLE_1 = 1 # |-
ANGLE_2 = 2 # -|
ANGLE_3 = 3 # _|
LINE_0  = 4 # ___
LINE_1  = 5 # |

TILES = [ANGLE_0, ANGLE_1, ANGLE_2, ANGLE_3, LINE_0, LINE_1]

def init_state(n, m):
	return [[True for _ in range(m)] for _ in range(n)]

def freeze(state):
	return tuple(map(tuple, state))

def find_first_free_cell(state, row0, col0):
	for row in range(row0, len(state)):
		for col in range(col0, len(state[row])):
			if state[row][col]:
				return (row, col)
	return None

def in_bounds(i, j, state):
	return (0 <= i and i < len(state)
		and 0 <= j and j < len(state[i]))

def safe_check(i, j, state):
	return in_bounds(i, j, state) and state[i][j]

# WLOG (i, j) is upper-left corner of tile
def can_place(tile, i, j, state):
	ret = True
	if tile == ANGLE_0: # |_
		ret &= safe_check(i  , j  , state)
		ret &= safe_check(i+1, j  , state)
		ret &= safe_check(i+1, j+1, state)
	elif tile == ANGLE_1: # |-
		ret &= safe_check(i  , j  , state)
		ret &= safe_check(i+1, j  , state)
		ret &= safe_check(i  , j+1, state)
	elif tile == ANGLE_2: # -|
		ret &= safe_check(i  , j  , state)
		ret &= safe_check(i  , j+1, state)
		ret &= safe_check(i+1, j+1, state)
	elif tile == ANGLE_3: # _|
		ret &= safe_check(i  , j  , state)
		ret &= safe_check(i+1, j  , state)
		ret &= safe_check(i+1, j-1, state)
	elif tile == LINE_0: # ___
		ret &= safe_check(i  , j  , state)
		ret &= safe_check(i  , j+1, state)
		ret &= safe_check(i  , j+2, state)
	elif tile == LINE_1: # |
		ret &= safe_check(i  , j  , state)
		ret &= safe_check(i+1, j  , state)
		ret &= safe_check(i+2, j  , state)
	return ret

def place(tile, i, j, state):
	if tile == ANGLE_0: # |_
		state[i  ][j  ] = False
		state[i+1][j  ] = False
		state[i+1][j+1] = False
	elif tile == ANGLE_1: # |-
		state[i  ][j  ] = False
		state[i+1][j  ] = False
		state[i  ][j+1] = False
	elif tile == ANGLE_2: # -|
		state[i  ][j  ] = False
		state[i  ][j+1] = False
		state[i+1][j+1] = False
	elif tile == ANGLE_3: # _|
		state[i  ][j  ] = False
		state[i+1][j  ] = False
		state[i+1][j-1] = False
	elif tile == LINE_0: # ___
		state[i  ][j  ] = False
		state[i  ][j+1] = False
		state[i  ][j+2] = False
	elif tile == LINE_1: # |
		state[i  ][j  ] = False
		state[i+1][j  ] = False
		state[i+2][j  ] = False

dp_cache = dict()
def n_tilings(state, row0=0, col0=0):
	frozen_state = freeze(state)
	if frozen_state in dp_cache:
		return dp_cache[frozen_state]

	first_free_cell = find_first_free_cell(state, row0, col0)
	if first_free_cell is None:
		return 1
	(row, col) = first_free_cell

	n = 0
	for tile in TILES:
		if can_place(tile, row, col, state):
			state_copy = copy.deepcopy(state)
			place(tile, row, col, state_copy)
			n += n_tilings(state_copy, row, col)
	
	dp_cache[frozen_state] = n
	return n

print n_tilings(init_state(12, 9))
