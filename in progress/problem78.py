
import sys
import pdb
import math

def access_map(dp_table, desired_sum, max_usable_coin):
    if desired_sum == -1:
        return 0
    if desired_sum in [0, 1]:
        return 1
    if max_usable_coin == 1:
        return 1

    if max_usable_coin > desired_sum:
        return dp_table[desired_sum][desired_sum]
    else:
        return dp_table[desired_sum][max_usable_coin]

def calc_num_ways(dp_table, desired_sum, max_usable_coin):
    num_ways = 0
    for coin in xrange(1, max_usable_coin+1):
        updated_sum = max(-1, desired_sum - coin)
        num_ways += access_map(dp_table, updated_sum, coin)
    return num_ways

dp_table = [[]]
desired_sum = 0
while True:
    desired_sum += 1
    dp_table.append([-1])
    for max_usable_coin in xrange(1,desired_sum+1):
        num_ways = calc_num_ways(dp_table, desired_sum, max_usable_coin)
        dp_table[desired_sum].append(num_ways)

    print 'P(', desired_sum, ') = ', dp_table[desired_sum][desired_sum]
    if dp_table[desired_sum][desired_sum] % 1000000 == 0:
        break