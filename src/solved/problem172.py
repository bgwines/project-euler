
import copy

memodict = dict()

def sig(counts):
    cs = [0,0,0,0]
    for c in counts:
        cs[c] += 1
    return tuple(cs)

def f(counts, d):
    if (d == 18):
        return 1

    if sig(counts) in memodict:
        return memodict[sig(counts)]

    num_lists = 0
    for i in xrange(0, 9+1):
        if (counts[i] == 0):
            continue

        counts[i] -= 1
        num_lists += f(counts, d+1)
        counts[i] += 1

    memodict[sig(counts)] = num_lists
    return num_lists


if __name__ == "__main__":
    print (9 * f([3,3,3,3,3,3,3,3,3,3], 0)) / 10