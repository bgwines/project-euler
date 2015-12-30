
import pdb
import pyprimes

'''
Let S = {2, 3, 5, ..., 4999} be the set of prime numbers less than 5000.

Find the number of subsets of S, the sum of whose elements is a prime number. Enter the rightmost 16 digits as your answer.
'''

'''
1548136 # desired sum of S
...
8 |
7 |x x   .   .
6 |x x   x   x
5 |x .   .   .
4 |x x   x   x
3 |x .   .   .
2 |. .   .   .
1 |
0 |
  ######
   2 3   5   7...4999 # max usable prime (sUB)
'''

def takeWhile(p, xs):
    result = []
    for x in xs:
        if not p(x):
            break
        result.append(x)
    return result

sUB = 50 #5000

s = takeWhile(lambda p: p < sUB, pyprimes.primes())

# | Number of ways to form `n` as a summation of primes less than `maxPrime`
memo = dict()
@profile
def numFormations(n, maxPrime):
    if (n == 0):
        return 1 # formation is the empty set

    if (n, maxPrime) in memo:
        return memo[(n, maxPrime)]

    result = 0
    for p in s:
        if p >= maxPrime or p > n:
            break
        result += numFormations(n - p, p)
        result %= 10000000000000000
    memo[(n, maxPrime)] = result
    return result

@profile
def main():
    print "S = takeWhile (< ", sUB, ") P.primes"
    print "Number of subsets of S that sum to a prime number: "

    result = 0
    ssum = sum(s)
    maxUsablePrime = max(s)
    for p in pyprimes.primes():
        # `sum s` is for the subset that is `S`; the maximal subset
        if not p <= ssum:
            break
        r = numFormations(p, maxUsablePrime + 1)
        # print 'f(',p,') = ', r
        # `+ 1` because of noninclusiveness
        result += r#numFormations(p, p + 1)
        result %= 10000000000000000
    print result
    print 'cache size: ', len(memo)

if __name__ == '__main__':
    main()
