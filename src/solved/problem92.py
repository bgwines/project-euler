# A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
# 
# For example,
# 
# 44 → 32 → 13 → 10 → 1 → 1
# 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
# 
# Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
# 
# How many starting numbers below ten million will arrive at 89?

def f(n):
	sum_of_squares = 0
	for d in str(n):
		sum_of_squares += int(d)**2
	return sum_of_squares

def not_cycled_yet(n):
	return (n != 1) and (n != 89)

def f_seq(n):
	if n == 0:
		return []
	seq = [n]
	while not_cycled_yet(n):
		n = f(n)
		seq.append(n)
	return seq

def calc_terminations(seq, precalced):
	terminal = seq[len(seq)-1]
	for e in seq:
		if e not in precalced:
			precalced[e] = terminal
		else:
			break
	return terminal

def calc(UB):
	terminations = {}
	eightynines = 0
	for n in xrange(UB):
		n += 1
		seq = f_seq(n)
		terminations[n] = calc_terminations(seq, terminations)
		if terminations[n] == 89:
			eightynines += 1
	return eightynines