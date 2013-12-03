{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

--mults_under_n :: (Integer a) => b -> [a]
mults_under_param n =
	filter
		(\x -> or (map (\y -> x `mod` y == 0) [3,5]))
		[1..n-1]

s = sum $ mults_under_param 1000