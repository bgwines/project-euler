
import copy
import itertools
import pdb
import math

def is_int(n):
	return n % 1 == 0

def is_square(n):
	return is_int(math.sqrt(n))

def get_elem_counts(l):
	d = {}
	for ch in str(l):
		if ch in d:
			d[ch] += 1
		else:
			d[ch] = 1
	return frozenset([(k, v) for k, v in d.items()])

def slice_out(l, i):
    return l[0:i] + l[i+1:]

def gen_assignments_rec(src, so_far, remaining_digits, assignments):
    if len(src) == 0:
        assignments.append(so_far)
        return

    for i, d in enumerate(remaining_digits):
        so_far_copy = copy.deepcopy(so_far)
        so_far_copy[src[0]] = d
        remaining_digits_copy = slice_out(remaining_digits, i)
        gen_assignments_rec(src[1:], so_far_copy, remaining_digits_copy, assignments)

def gen_assignments(s):
    assignments = []
    s = list(set(s))
    gen_assignments_rec(s, {}, [0,1,2,3,4,5,6,7,8,9], assignments)
    return assignments

def apply_number_assignment(assignment, s):
	n = 0
	for i, ch in enumerate(s):
		n *= 10
		n += assignment[ch]
	return n

def is_valid_assignment(a, b, assignment):
	return (assignment[a[0]] != 0) and (assignment[b[0]] != 0)

def get_anagram_sets_dict(words):
	anagram_sets_dict = {}
	for word in words:
		elem_counts = get_elem_counts(word)
		if elem_counts not in anagram_sets_dict:
			anagram_sets_dict[elem_counts] = []
		anagram_sets_dict[elem_counts].append(word)
	return anagram_sets_dict

def get_anagram_sets(anagram_sets_dict):
	anagram_sets = []
	for k, anagram_set in anagram_sets_dict.items():
		if len(anagram_set) == 2:
			anagram_sets.append(anagram_set)
	return anagram_sets

words = []
for line in open("words.txt", "r"):
	words.append(line.strip())

anagram_sets_dict = get_anagram_sets_dict(words)

anagram_sets = get_anagram_sets(anagram_sets_dict)

square_pairs = []
i = 0
for anagram_set in anagram_sets:
	i += 1
	print i, '/', len(anagram_sets), ' - ', len(anagram_set[0])
	j = 0
	all_assignments = gen_assignments(anagram_set[0])
	for assignment in all_assignments:
		j += 1
		if len(anagram_set[0]) > 6:
			print '\tassignment #', j, '/', len(all_assignments)  
		if len(anagram_set) != 2:
			pdb.set_trace()
		if not is_valid_assignment(anagram_set[0], anagram_set[1], assignment):
			continue
		if is_square(apply_number_assignment(assignment, anagram_set[0])):
			if is_square(apply_number_assignment(assignment, anagram_set[1])):
				square_pairs.append((anagram_set, assignment))

print square_pairs

square_pairs.sort(key=lambda (a,b):-1*len(b))
max_len_pair = square_pairs[0]
print max(
	apply_number_assignment(max_len_pair[1], max_len_pair[0][0]),
	apply_number_assignment(max_len_pair[1], max_len_pair[0][1])
)
