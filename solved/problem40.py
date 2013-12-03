
import pdb

def index(s, i):
	return s[i-1]

s = ""
for i in xrange(1,1000000):
	s += str(i)

prod = 1
l = [1,10,100,1000,10000,100000,1000000]
for e in l:
	prod *= int(index(s, e))
print prod