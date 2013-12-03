{-The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?-}

import Prime
import Perms

l = [1..9]

smallprimes = filter (\n -> (length $ show n) == 4) $ takeWhile (\n -> (length $ show n) <= 4) primes

seqprimes = filter 
	(\n -> ((n + 3330)		  `elem` smallprimes) && 
		   ((n + 3330 + 3330) `elem` smallprimes)) 
	smallprimes

is_perm_triple (a,b,c) =
	(show b) `elem` gen_perms (show a) &&
	(show b) `elem` gen_perms (show a)

seqs = map (\n -> (n, n+3330, n+3330+3330)) seqprimes

perms = filter is_perm_triple seqs