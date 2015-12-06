
import itertools
import math
import pdb
import pyprimes
import sys

def get_primes_leq(n):
    primes = []
    for p in pyprimes.primes():
        if p > n:
            break
        primes.append(p)
    return primes

def access_map(dp_table, desired_sum, max_value):
    if desired_sum == -1:
        return 0
    if desired_sum == 0:
        return 1
    if desired_sum == 1:
        return 0
    if max_value == 1:
        return 0

    if max_value > desired_sum:
        return dp_table[(desired_sum, get_primes_leq(desired_sum)[-1])]
    else:
        return dp_table[(desired_sum, max_value)]

def calc_num_ways(dp_table, desired_sum, max_value):
    num_ways = 0
    for p in get_primes_leq(max_value):
        updated_sum = max(-1, desired_sum - p)
        num_ways += access_map(dp_table, updated_sum, p)
    return num_ways

dp_table = {}
for desired_sum in itertools.count(1):
    for max_value in get_primes_leq(desired_sum):
        num_ways = calc_num_ways(dp_table, desired_sum, max_value)
        dp_table[(desired_sum, max_value)] = num_ways

    if desired_sum != 1:
        if 5000 < dp_table[(desired_sum, get_primes_leq(desired_sum)[-1])]:
            print desired_sum
            sys.exit(0)