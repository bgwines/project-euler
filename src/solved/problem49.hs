{-The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?-}

import qualified Zora.Math as ZMath
import qualified Zora.List as ZList

seqprimes :: [Integer]
seqprimes = filter 
	(\p -> ((p + 3330)		  `elem` smallprimes) && 
		   ((p + 3330 + 3330) `elem` smallprimes)) 
	smallprimes
	where
		smallprimes :: [Integer]
		smallprimes
			= filter    (\n -> (length $ show n) == 4)
			. takeWhile (\n -> (length $ show n) <= 4)
			$ ZMath.primes

is_permutation_triple :: (Integer, Integer, Integer) -> Bool
is_permutation_triple (a, b, _) = (show b) `elem` (ZList.permutations $ show a)

seqprime_triples :: [(Integer, Integer, Integer)]
seqprime_triples = map (\n -> (n, n+3330, n+3330+3330)) seqprimes

perms :: [(Integer, Integer, Integer)]
perms = filter is_permutation_triple seqprime_triples

main :: IO ()
main = do
	putStrLn . show $ perms