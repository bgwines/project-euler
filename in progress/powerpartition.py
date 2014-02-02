
import pdb
import copy

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

ll = [[], [0], [0,1], [0,1,2], [0,1,2,3], [0,1,2,3,4]]#, [0,1,2,3,4,5], [0,1,2,3,4,5,6]]
for l in ll:
	print ''
	for e in powerpartition(l):
		print '\t', e