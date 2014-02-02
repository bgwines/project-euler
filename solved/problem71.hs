{-
Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that 2/5 is the fraction immediately to the left of 3/7.

By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.
-}

import List
import qualified Data.List
import qualified Data.Ord

ub = 1000000

get_triples :: Double -> [(Double, Double, Double)]
get_triples d = map form_triple [max_n, max_n-1 .. 1]
	where
		max_n = (fromRealFrac . ceiling) (d * (3/7))
		form_triple n = (n, d, n / d)

get_best_triple_for_denominator :: Double -> Maybe (Double, Double, Double)
get_best_triple_for_denominator d = find ((< 3/7) . trd3) $ get_triples d

best_nds :: [Maybe (Double, Double, Double)]
best_nds = [get_best_triple_for_denominator d | d <- [ub,ub-1..1]]

best_triple = Data.List.maximumBy (Data.Ord.comparing trd3) triples
	where triples = map from_just $ filter (/= Nothing) best_nds

main = do
	(putStrLn . show) $ fst3 best_triple