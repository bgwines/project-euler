
import math

UB = 10000

def elem_counts(s):
	d = {}
	for ch in str(s):
		if ch in d:
			d[ch] += 1
		else:
			d[ch] = 1
	return (tuple(d.keys()), tuple(d.values()))

d = {}
for e in xrange(UB):
	cube = e**3
	ec = elem_counts(cube)
	if ec in d:
		d[ec].append(e)
	else:
		d[ec] = [e]

for k,v in d.items():
	if len(v) == 5:
		print k
		print v
		for e in v:
			print e**3
		print ''