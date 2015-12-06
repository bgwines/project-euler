{-
Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 3 fractions between 1/3 and 1/2.

How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?
-}

import List
import qualified Data.List
import qualified Data.Ord

ub = 12000

gt_frac = 1/2
lt_frac = 1/3

get_fracs :: Double -> [Double]
get_fracs d = [n / d | n <- [min_n .. max_n], (gcd (round n) (round d)) == 1]
	where
		min_n = (fromRealFrac . floor  ) (d * lt_frac) - 1
		max_n = (fromRealFrac . ceiling) (d * gt_frac) + 1

in_range :: Double -> Bool
in_range n = (n < gt_frac) && (n > lt_frac)

count_in_range_fracs_for_denominator :: Double -> Int
count_in_range_fracs_for_denominator d = length $ filter in_range $ get_fracs d

n_in_range_fracs :: Int
n_in_range_fracs = sum [count_in_range_fracs_for_denominator d | d <- [1..ub]]

main = do
	(putStrLn . show) n_in_range_fracs