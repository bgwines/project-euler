{-
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
-}

fibs :: [Integer]
fibs = 0 : 1 : (zipWith (+) fibs (tail fibs))

even_smallfibs :: [Integer]
even_smallfibs = filter even . takeWhile (< 4000000) $ fibs

main :: IO ()
main = do
	putStrLn . show $ sum even_smallfibs