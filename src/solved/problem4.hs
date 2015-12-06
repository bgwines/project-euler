{-
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

import Zora.List

forms_mult_palindrome_pair :: (Integer, Integer) -> Bool
forms_mult_palindrome_pair (a,b) = is_palindrome . show $ a * b

mult_palindrome_pairs :: [(Integer, Integer)]
mult_palindrome_pairs
	= filter forms_mult_palindrome_pair
	$ zip [100..999] [100..999]

mults :: [Integer]
mults = map (\(a,b) -> a*b) mult_palindrome_pairs

main :: IO ()
main = do
	putStrLn . show $ maximum mults