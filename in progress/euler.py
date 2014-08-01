
import copy
import pyprimes
import pdb

def zip_with(xs, ys, f):
    l = []
    for i in xrange(min(len(xs), len(ys))):
        l.append(f(xs[i], ys[i]))
    return l

def divides_both(d, n, m):
    if (n % d == 0) and (m % d == 0):
        return 1
    else:
        return 0

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

def get_elem_counts(s):
    d = {}
    for ch in str(s):
        if ch in d:
            d[ch] += 1
        else:
            d[ch] = 1
    return (tuple(d.keys()), tuple(d.values()))

def slice_out(l, i):
    return l[0:i] + l[i+1:]

def gen_assignments_rec(src, so_far, remaining_digits, assignments):
    if len(src) == 0:
        assignments.append(so_far)
        return

    for i, d in enumerate(remaining_digits):
        so_far_copy = copy.deepcopy(so_far)
        so_far_copy[src[0]] = d
        remaining_digits_copy = slice_out(remaining_digits, i)
        gen_assignments_rec(src[1:], so_far_copy, remaining_digits_copy, assignments)

def gen_assignments(s):
    assignments = []
    s = list(set(s))
    gen_assignments_rec(s, {}, [0,1,2,3,4,5,6,7,8,9], assignments)
    return assignments

def gen_perms_rec(src, so_far, perms):
    if len(src) == 0:
        perms.append(so_far)

    for i in xrange(len(src)):
        src_sliced_out = src[0:i] + src[i+1:]
        so_far_copy = copy.deepcopy(so_far)
        so_far_copy.append(src[i])
        gen_perms_rec(src_sliced_out, so_far_copy, perms)

def gen_perms(l):
    perms = []
    gen_perms_rec(l, [], perms)
    return perms

def filter(P, l):
    ll = []
    lenl = len(l)
    for i, e in enumerate(l):
            if i % 1000 == 0:
                    print i, '/', lenl
            if P(e):
                    ll.append(e)
    return ll

def get_duplicate_indices(l):
    elems_to_indices = {}
    for i, e in enumerate(l):
        if e not in elems_to_indices:
            elems_to_indices[e] = [i]
        else:
            elems_to_indices[e].append(i)

    duplicate_indices = set()
    for elem, indices in elems_to_indices.items():
        if len(indices) > 1:
            for index in indices:
                duplicate_indices.add(index)
    print elems_to_indices
    print duplicate_indices


def coprime(n, m):
    less = [e for e in xrange(2, min (n, m))]
    n_shared_divisors = sum([divides_both(d, n, m) for d in less])
    return n_shared_divisors == 0

def calc_num_factors(n):
    count = 1
    for (p, a) in pyprimes.factorise(n):
        count *= (a + 1)
    return count

def product(l):
    prod = 1
    for e in l:
        prod *= e
    return prod

def flatten_1(l):
    ll = []
    for e in l:
        for ee in e:
            ll.append(ee)
    return ll

def minimum(l):
    min_so_far = None
    for e in l:
        if (min_so_far == None) or (e < min_so_far):
            min_so_far = e
    return min_so_far
    
def get_products(l):
    prods = [1]
    for ns in l:
        new_prods = map(lambda x: [x*n for n in ns], prods)
        new_prods = flatten_1(new_prods)
        for new_prod in new_prods:
            prods.append(new_prod)

    return prods

def get_proper_divisors(n):
    divisors = get_divisors(n)
    divisors.remove(n)
    return divisors

def get_divisors(n):
    prime_powers = []
    for p, a in pyprimes.factorise(n):
        l = []
        for expnt in xrange(1, a+1):
            l.append(p ** expnt)
        prime_powers.append(l)

    powerset = gen_powerset(prime_powers)

    prods = map(get_products, powerset)
    s = set()
    for prod_list in prods:
        for prod in prod_list:
            s.add(prod)
    return list(s)

def gen_powerset_rec(src, so_far, powerset):
    if len(src) == 0:
        powerset.append(so_far)
        return

    e = src[0]
    src = src[1:]

    gen_powerset_rec(src, copy.deepcopy(so_far), powerset)

    so_far.append(e)
    gen_powerset_rec(src, so_far, powerset)

def gen_powerset(src):
    powerset = []
    gen_powerset_rec(src, [], powerset)
    return powerset

def gen_subsets_of_size_rec(src, so_far, powerset, size):
    if len(so_far) == size:
        powerset.append(so_far)
        return
    if len(src) == 0:
        return

    e = src[0]
    src = src[1:]

    gen_subsets_of_size_rec(src, copy.deepcopy(so_far), powerset, size)

    so_far.append(e)
    gen_subsets_of_size_rec(src, so_far, powerset, size)

def gen_subsets_of_size(src, size):
    powerset = []
    gen_powerset_rec(src, [], powerset, size)
    return powerset