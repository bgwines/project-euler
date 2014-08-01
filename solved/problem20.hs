{-
n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
-}

import Data.Char

fact :: Integer -> Integer
fact n = product [1..n]

digit_sum :: Int
digit_sum = sum . map digitToInt . show . fact $ 100

main :: IO ()
main = do
	putStrLn . show $ digit_sum