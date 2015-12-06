
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

prefix_sums = {} #up to each p
prefix_sums[tuple([])] = 0
curr_sum = 0
for p in primes:
    curr_sum += p
    prefix_sums[p] = curr_sum

index = 0
max_len = -1
max_len_subseq = []
for i in xrange(0, len(primes)):
    for j in xrange(i, len(primes)):
    	index += 1
    	if index % 1000000 == 0:
    		print 'progress: ', float(index) / UB**2, '% (', float(index), '/', UB**2, ')'
    		print 'time: ', str(datetime.timedelta(seconds=(time.time() - startTime)))

        sum_to_i = prefix_sums[primes[i]]
        sum_to_j = prefix_sums[primes[j]]
        sum_from_i_to_j = sum_to_j - sum_to_i + primes[i]

        if sum_from_i_to_j < UB and pyprimes.isprime(sum_from_i_to_j):
        	curr_length = len(primes[i:j+1])
        	if max_len < curr_length:
        		print curr_length, ' : ',
        		print primes[i:j+1]
        		print ''
        		max_len = curr_length
        		max_len_subseq = primes[i:j+1]

print ''
print ''
print max_len_subseq, ' - len ', len(max_len_subseq), ', sum ', sum(max_len_subseq)