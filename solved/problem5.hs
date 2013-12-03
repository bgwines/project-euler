{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

import Data.List

is_prime_rec :: (Num a, Integral a) => a -> a -> Bool
is_prime_rec n k
	| n == 1 = False
	| k < 2 = True
	| ((n `mod` k) == 0) = False
	| otherwise = is_prime_rec n (k-1)

is_prime :: (Num a, Integral a) => a -> Bool
is_prime n = is_prime_rec n (n-1)

divisible_for_all_in_list :: (Num a, Integral a) => [a] -> a -> Bool
divisible_for_all_in_list l n =
	and [n `mod` k == 0 | k <- l]

-- 1,2,3,4,5,8,9,10 all have multiples in l
l = [20,19,18,17,16,15,14,13,12,11]

product_of_primes = product $ filter is_prime l
sought = find (divisible_for_all_in_list l)
	$ map (\x -> x * product_of_primes) [1..]