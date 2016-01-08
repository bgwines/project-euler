''' Prime Subset Sums
Problem 249
Let S = {2, 3, 5, ..., 4999} be the set of prime numbers less than 5000.

Find the number of subsets of S, the sum of whose elements is a prime number.
Enter the rightmost 16 digits as your answer.
'''

import pdb
import pyprimes

'''
1548136 # desired sum of S
...
8 |
7 |
6 |
5 |  <8 billion entries>
4 |
3 |
2 |
1 |
0 |______________________
   1 2 3 4 5 6 7 ... 4999 # max usable prime (sUB)
'''

def takeWhile(p, xs):
    result = []
    for x in xs:
        if not p(x):
            break
        result.append(x)
    return result

sUB = 200 #5000

s = takeWhile(lambda p: p < sUB, pyprimes.primes())

hits = 0
misses = 0

# | Number of ways to form `n` as a summation of primes less than `maxPrime`
# (note the non-inclusiveness in the second parameter)
memo = dict()
# @profile
def numFormations(n, maxPrime):
    if (n == 0):
        return 1 # formation is the empty set

    global s
    global hits
    global misses

    cachedNumFormations = memo.get((n, maxPrime))
    if cachedNumFormations is not None:
        hits += 1
        return cachedNumFormations
    misses += 1

    result = 0
    for p in s:
        if p >= maxPrime or p > n:
            break
        result += numFormations(n - p, p)
        result %= 10000000000000000
    # print "f(", n, ",", maxPrime, ") = ", result
    memo[(n, maxPrime)] = result
    return result

# @profile
def main():
    global s
    global sUB
    global hits
    global misses

    print "S = takeWhile (< ", sUB, ") P.primes"
    print "Number of subsets of S that sum to a prime number: ",

    result = 0
    ssum = sum(s)
    maxUsablePrime = max(s)
    for p in pyprimes.primes():
        # `sum s` is for the subset that is `S`; the maximal subset
        if not p <= ssum:
            break

        # `+ 1` because of noninclusiveness
        result += numFormations(p, min(p + 1, maxUsablePrime + 1))
        result %= 10000000000000000
    print result
    # print 'cache size: ', len(memo)
    print 'hits: ', hits
    print 'misses: ', misses

if __name__ == '__main__':
    main()
