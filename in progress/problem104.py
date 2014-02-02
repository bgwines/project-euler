import pdb
import math

def is_pandigital(s):
	return sorted(s) == ['1', '2', '3', '4', '5', '6', '7', '8', '9']

def valid(e):
	n_digits = int(math.floor(math.log(e,10))+1)
	first_9_digits = int(math.floor(e / (10**(n_digits-9))))
	last_9_digits = e % (10**9)
	return is_pandigital(str(first_9_digits)) and is_pandigital(str(last_9_digits))

def f(i, e):
	if e >= 123456789:
		if valid(e):
			print '#', i+1, ': ', e
			print ''
			return True
	return False

def find(UB):
	a = 1
	b = 1
	i = 2
	# print 1, ':', 1
	# print 2, ':', 1
	for j in xrange(UB):
	    tmp = a + b
	    if a < b:
	        a = tmp
	    else:
	        b = tmp
	    if f(i, tmp):
	    	break
	    if i % 10000 == 0:
	    	print '#', i
	    i += 1

find(1000000)