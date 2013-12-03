
import pdb
import pyprimes

def fast_euler_phi(n, totients):
	if n == 2:
		return 1
	if n % 2 == 0:
		if (n / 2) % 2 == 0:
			return 2 * totients[n / 2]
		else:
			return 1 * totients[n / 2]
	else:
		if pyprimes.isprime(n):
			return n-1
		else:
			phi = 1
			for (p, a) in pyprimes.factorise(n):
				phi *= (p ** (a-1)) * (p-1)
			return phi

totients = [0, 0]
fs = [0, 0] #filler for 0 and 1 indices
for n in xrange(2,1000000):
	if n % 10000 == 0:
		print n, '/', 1000000
	phi = fast_euler_phi(n, totients)
	totients.append(phi)

	n *= 1.0
	fs.append(n / phi)
	#print 'phi(', n, ') = ', phi, ', n/phi(n) = ', n / phi

max_f = -1
max_i = 0
for i, f in enumerate(fs):
	if f > max_f:
		max_f = f
		max_i = i

print 'max: fs[', max_i, '] = ', max_f