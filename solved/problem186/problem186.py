''' Here are the records from a busy telephone system with one million users:

RecNr	Caller	Called
1	200007	100053
2	600183	500439
3	600863	701497
...	...	...
The telephone number of the caller and the called number in record n are Caller(n) = S2n-1 and Called(n) = S2n where S1,2,3,... come from the "Lagged Fibonacci Generator":

For 1 <= k <= 55, Sk = [100003 - 200003k + 300007k3] (modulo N_PEOPLE)
For 56 <= k, Sk = [Sk-24 + Sk-55] (modulo N_PEOPLE)

If Caller(n) = Called(n) then the user is assumed to have misdialled and the call fails; otherwise the call is successful.

From the start of the records, we say that any pair of users X and Y are friends if X calls Y or vice-versa. Similarly, X is a friend of a friend of Z if X is a friend of Y and Y is a friend of Z; and so on for longer chains.

The Prime Minister's phone number is PM. After how many successful calls, not counting misdials, will 99 percent of the users (including the PM) be a friend, or a friend of a friend etc., of the Prime Minister? '''

import disjoint_set_forest as DSF
import itertools

N_PEOPLE = 1000000
PM = 524287

lfg_cache = dict()
def lfg(k):
	if k in lfg_cache:
		return lfg_cache[k]

	if 1 <= k and k <= 55:
		lfg_cache[k] = (100003 - 200003*k + 300007*(k**3)) % N_PEOPLE
	else:
		lfg_cache[k] = (lfg(k-24) + lfg(k-55)) % N_PEOPLE
	return lfg_cache[k]

def is_misdial(call):
	return call[0] == call[1]

dsf = DSF.DisjointSetForest([x for x in xrange(0, N_PEOPLE)])

n_calls = 0
for n in itertools.count(1):
	call = (lfg(2*n - 1), lfg(2*n))
	if is_misdial(call):
		continue
	n_calls += 1

	dsf.union(call[0], call[1])

	if dsf.set_size(PM) >= (0.99 * N_PEOPLE):
		print n_calls
		break

