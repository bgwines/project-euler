
import pdb
import math

def head(l):
    return l[1:]

def get_arr_prefixes(list):
    prefixes = []
    for i in xrange(len(list)+1):
        prefixes.append(list[0:i])
    return head(prefixes)

def create_map(l):
    m = {}
    for e in l:
        if e in m:
            m[e] = m[e] + 1
        else:
            m[e] = 1
    return m
    
def get_all_splits(n):
    arr = []
    for i in xrange(n/2 + 1):
        arr.append((i, n-i))
    return arr

def get_coins_leq(n, coins):
    coins_leq = []
    for coin in coins:
        if coin > n:
            break
        coins_leq.append(coin)
    return tuple(coins_leq)

def calc_num_ways(dp_table, desired_sum, usable_coins):
    if desired_sum == -1:
        return 0
    if desired_sum in [0, 1]:
        return 1
    if usable_coins == [1]:
        return 1

    num_ways = 0
    for coin in usable_coins:
        coins_leq = get_coins_leq(coin, usable_coins)
        updated_sum = max(-1, desired_sum - coin)
        num_ways += dp_table[(updated_sum, coins_leq)]

    return num_ways

def custom_cmp(a, b):
    if a[0] > b[0]:
        return 1
    elif a[0] == b[0]:
        if a[1] > b[1]:
            return 1
        elif a[1] == b[1]:
            return 0
        else:
            return -1
    else:
        return -1

def printmap(m):
    keys = []
    for k in m:
        keys.append(k)

    keys.sort(custom_cmp)

    for k in keys:
        print k, ' - ', m[k]

COINS = []
for i in xrange(1,99+1):
    COINS.append(i)
DESIRED_SUMS = []
for i in xrange(-1, 100+1):
    DESIRED_SUMS.append(i)

dp_table = {} #(a, b), a = sum, b = coin_prefix

for i, desired_sum in enumerate(DESIRED_SUMS):
    #pdb.set_trace()
    print desired_sum
    for _, usable_coins in enumerate(get_arr_prefixes(COINS)):
        dp_table[(desired_sum, tuple(usable_coins))] = \
            calc_num_ways(dp_table, desired_sum, usable_coins)
    #print ''

printmap(dp_table)