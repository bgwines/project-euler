{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

import Data.List
import Prime

divisible_for_all_in_list :: [Integer] -> Integer -> Bool
divisible_for_all_in_list l n =
	and [n `mod` k == 0 | k <- l]

-- 1,2,3,4,5,8,9,10 all have multiples in l
l :: [Integer]
l = [20,19,18,17,16,15,14,13,12,11]

product_of_primes :: Integer
product_of_primes = product $ filter is_prime l

divider :: Integer
divider = (\(Just x) -> x) $
	find (divisible_for_all_in_list l) products_of_primes
		where products_of_primes = map (* product_of_primes) [1..]

main = do
	(putStrLn . show) $  divider