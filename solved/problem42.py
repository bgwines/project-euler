
import pdb

def f(s):
    return sum([1+ord(ch)-ord('A') for ch in s])

def is_triangle(s, triangles):
	value = f(s)
	for t in triangles:
		#print t
		if t == value:
			return True
		if t > value:
			return False

def tri(n):
    return 0.5 * n * (n+1)

triangles = [int(tri(i)) for i in xrange(1,100)]

words = []
lines = open("words.txt", "r")
for word in lines:
	words.append(word.strip())

tri_words = []
for word in words:
	if is_triangle(word, triangles):
		tri_words.append(word)

pdb.set_trace()
print len(tri_words)