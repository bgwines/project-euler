''' Binary Circles
Problem 265
2N binary digits can be placed in a circle so that all the N-digit clockwise subsequences are distinct.

For N=3, two such circular arrangements are possible, ignoring rotations:

p265_BinaryCircles.gif

For the first arrangement, the 3-digit subsequences, in clockwise order, are:
000, 001, 010, 101, 011, 111, 110 and 100.

Each circular arrangement can be encoded as a number by concatenating the binary digits starting with the subsequence of all zeros as the most significant bits and proceeding clockwise. The two arrangements for N=3 are thus represented as 23 and 29:

00010111_2 = 23
00011101_2 = 29
Calling S(N) the sum of the unique numeric representations, we can see that S(3) = 23 + 29 = 52.

Find S(5).

in progress/ [master] > time python problem265.py
209110240768

real    0m18.559s
user    0m18.356s
sys     0m0.157s
'''

import copy

n = 4

def can_append(d, seq):
    covered = set()
    for i in xrange(0, len(seq) - n + 1):
        covered.add(tuple(seq[i:(i+n)]))
    new = seq[(len(seq)-n+1):]
    new.append(d)
    return tuple(new) not in covered

def cycle_until_zeros_at_start(seq):
    while any(d == 1 for d in seq[0:n]):
        seq = seq[1:] + [seq[0]]
    return seq

def valid(seq):
    seq = seq + seq[0:(n-1)]
    covered = set()
    for i in xrange(0, len(seq) - n + 1):
        subseq = tuple(seq[i:(i+n)])
        if subseq in covered:
            return False
        covered.add(subseq)
    return True

memo = dict()
def form_de_bruijn_seqs(length, o, z):
    if length == 1:
        if o == 0 and z == 1: return [[0]]
        if o == 1 and z == 0: return [[1]]
        raise Exception('fatal: weird trio: ' + str((length, o, z)))

    cached = memo.get((length, o, z))
    if cached is not None:
        return cached

    need0s = form_de_bruijn_seqs(length - 1, o    , z - 1) if z > 0 else []
    need1s = form_de_bruijn_seqs(length - 1, o - 1, z    ) if o > 0 else []

    curr = []
    for need0 in need0s:
        if can_append(0, need0):
            curr.append(need0 + [0])
    for need1 in need1s:
        if can_append(1, need1):
            curr.append(need1 + [1])
    memo[(length, o, z)] = curr
    return curr

def list_to_int(seq):
    s = ''
    for d in cycle_until_zeros_at_start(seq):
        s += str(d)
    return int(s, 2)

def main():
    # 011..110 must be in all seqs; force it to be at end
    # for smaller initial params to recursion
    seqs = form_de_bruijn_seqs(2**n - n - 2, 2**(n - 1) - n, 2**(n - 1) - 2)
    for seq in seqs:
        seq.append(0)
        for _ in xrange(0, n):
            seq.append(1)
        seq.append(0)

    print sum(map(list_to_int, filter(valid, seqs)))

if __name__ == '__main__':
    main()
