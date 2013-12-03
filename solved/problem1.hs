{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

get_mults_of_3_and_3_under :: Integer -> [Integer]
get_mults_of_3_and_3_under n = filter divides_3_and_5 [1..n-1]
	where divides_3_and_5 m = 
		(m `mod` 3 == 0) &&
		(m `mod` 5 == 0)

main = do
	(putStrLn . show) $ sum $ get_mults_of_3_and_3_under 1000