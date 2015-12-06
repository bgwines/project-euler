from cvxpy import *
import numpy
import math
import pdb

def product(l):
	return reduce(lambda a, b: a * b, l)

def optimize(m):
	A = numpy.diag([exp for exp in range(1, m+1)])

	x = Variable(m)
	objective = Maximize(sum(A * log(x)))

	constraints = [sum(x) - m == 0]

	prob = Problem(objective, constraints)

	result = prob.solve()
	return x.value.A1.tolist()

def P(m):
	return product(
		map(
			lambda (x_i, i): x_i**i,
			zip(optimize(m), range(1, m+1))
		)
	)

print int(
	sum(
		map(
			math.floor,
			map(P, range(2, 15+1))
		)
	)
)