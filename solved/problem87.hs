{-
The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. In fact, there are exactly four numbers below fifty that can be expressed in such a way:

28 = 2^2 + 2^3 + 2^4
33 = 3^2 + 2^3 + 2^4
49 = 5^2 + 2^3 + 2^4
47 = 2^2 + 3^3 + 2^4

How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?
-}

import Euler
import Prime
import qualified Data.List
import qualified Data.Ord
import qualified Data.Set

ub = 50000000

primes_for_exponent :: Integer -> [Integer]
primes_for_exponent exp = takeWhile (\p -> p^exp < ub) primes

--                 ^2       ^3       ^4
base_combos :: [(Integer, Integer, Integer)]
base_combos = [(a, b, c)
	| a <- (takeWhile (\a -> a^2             < ub) $ primes_for_exponent 2)
	, b <- (takeWhile (\b -> a^2 + b^3       < ub) $ primes_for_exponent 3)
	, c <- (takeWhile (\c -> a^2 + b^3 + c^4 < ub) $ primes_for_exponent 4)]

unique_sums_hit_rec :: [(Integer, Integer, Integer)] -> Data.Set.Set Integer -> Integer
unique_sums_hit_rec [] set = toInteger $ Data.Set.size set
unique_sums_hit_rec triples set = 
	unique_sums_hit_rec (tail triples) modified_set
	where
		triple_sum =
			(fst3 $ head triples)^2 +
			(snd3 $ head triples)^3 +
			(trd3 $ head triples)^4
		modified_set = Data.Set.insert triple_sum set


unique_sums_hit :: Integer
unique_sums_hit = unique_sums_hit_rec base_combos Data.Set.empty

main = do
	(putStrLn . show) unique_sums_hit