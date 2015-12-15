{- Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are reversible. Leading zeroes are not allowed in either n or reverse(n).

There are 120 reversible numbers below one-thousand.

How many reversible numbers are there below one-billion (10^9)? -}

import qualified Zora.List as ZList

import qualified Data.Char as Char

reverse_number :: Integer -> Integer
reverse_number = read . reverse . show

all_digits_odd :: Integer -> Bool
all_digits_odd = all odd . map Char.digitToInt . show

reversible :: Integer -> Bool
reversible n = ((n `mod` 10) /= 0) && (all_digits_odd $ n + n')
	where
		n' :: Integer
		n' = reverse_number n

ub = 10^7

main :: IO ()
main = do
	putStrLn . show $ ub
	putStrLn . show $ ZList.count reversible [1..(ub - 1)]--(10^9 - 1)]