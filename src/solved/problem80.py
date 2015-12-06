
import pdb
from decimal import *

squares = [i**2 for i in range(1,10)]

def get_digit_sum(n):
	if n in squares:
		return 0
	else:
		sqrt_str = str(Decimal(n).sqrt())
		sqrt_str = sqrt_str[0] + sqrt_str[2:101]
		return sum([int(ch) for ch in sqrt_str])

getcontext().prec = 103

print sum([get_digit_sum(i) for i in range(1, 100)])