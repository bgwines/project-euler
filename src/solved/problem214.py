
import pyprimes

ub = 40000000

phi_memo = dict()
def euler_phi(n):
    if n == 0: return 0
    if n == 1: return 1
    if n == 2: return 1
    if n % 2 == 0:
        if (n / 2) % 2 == 0:
            phi_memo[n] = 2 * euler_phi(n / 2)
        else:
            phi_memo[n] = 1 * euler_phi(n / 2)
    else:
        if pyprimes.isprime(n):
            return n - 1
        else:
            phi = 1
            for (p, a) in pyprimes.factorise(n):
                phi *= (p ** (a - 1)) * (p - 1)
            phi_memo[n] = phi
    return phi_memo[n]

memo = dict()
def chain_length(n):
    if n == 1:
        return 1

    if n not in memo:
        memo[n] = 1 + chain_length(euler_phi(n))
    return memo[n]

def main():
    result = 0
    i = 0
    print 'upper bound: ', ub
    for p in pyprimes.primes():
        if p > ub:
            break
        if p < 9000000:
            continue
        i += 1

        if i % 50000 == 0:
            print i, '/', 1797049, '(', i / 1797049.0, ')'

        if chain_length(p - 1) == 24:
            result += p

    print result

if __name__ == '__main__':
    main()
