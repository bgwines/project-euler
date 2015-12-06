import pdb
import pyprimes
import time
import datetime

startTime = time.time()

UB = 1000000

subseqs_to_sums = {}

primes = []
for p in pyprimes.primes():
    if p >= UB:
        break
    primes.append(p)

primesset = set(primes)

subseq_sums = {}
best_sum_so_far = -1
best_subseq = []
b = False
index = 0
for length in xrange(len(primes), 0, -1):
    if b:
        break
    for starting_index in xrange(len(primes) - length + 1):
        index += 1
        if index % 1000000 == 0:
            print 'progress: ', float(index) / UB**2, '% (', float(index), '/', UB**2, ')'
            print 'time: ', str(datetime.timedelta(seconds=(time.time() - startTime)))

        ########################################
        s = 0
        if starting_index != 0:
            s = subseq_sums[(length+1, starting_index-1)] - primes[starting_index-1]
        else:
            if length == len(primes):
                s = sum(primes[starting_index : starting_index+length])
            else:
                s = subseq_sums[length+1, starting_index] - primes[starting_index+length]
        ########################################

        subseq_sums[(length, starting_index)] = s
        if s <= UB and s in primesset:
            print ''
            print primes[starting_index : starting_index+length],
            print ' - len ', length,
            print ', sum ', s
            b = True
            break