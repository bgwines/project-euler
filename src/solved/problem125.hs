
{- The palindromic number 595 is interesting because it can be written as the sum of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.

There are exactly eleven palindromes below one-thousand that can be written as consecutive square sums, and the sum of these palindromes is 4164. Note that 1 = 0^2 + 1^2 has not been included as this problem is concerned with the squares of positive integers.

Find the sum of all the numbers less than 10^8 that are both palindromic and can be written as the sum of consecutive squares. -}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Set as Set
import qualified Data.List as List

import Data.Maybe

import Control.Applicative

(<$*>) :: Applicative f => (a -> a -> b) -> f a -> f b
f <$*> l = f <$> l <*> l

ub :: Integer
ub = 10^8

-- [1..9999]
squares :: [Integer]
squares = takeWhile (< ub) . map (^2) $ [1..]

square_prefix_sums :: [Integer]
square_prefix_sums = scanl1 (+) squares

square_prefix_sum_set :: Set.Set Integer
square_prefix_sum_set = Set.fromList $ square_prefix_sums

consecutives :: Set.Set (Integer, Integer)
consecutives = Set.fromList $ zip square_prefix_sums (tail square_prefix_sums)

formable :: Integer -> Bool
formable n = any will_form square_prefix_sums
	where
		will_form :: Integer -> Bool
		will_form prefix_sum =
			if (n + prefix_sum) `Set.member` square_prefix_sum_set
			then not $ (prefix_sum, (n+prefix_sum)) `Set.member` consecutives
			else n `Set.member` square_prefix_sum_set -- from the start

		sqrt_perfect_square :: Integer -> Integer
		sqrt_perfect_square = toInteger . ceiling . sqrt . fromInteger

palindromes :: [Integer]
palindromes = filter (ZList.is_palindrome . show) . tail $ [1..ub] -- tail because 1 will be formable

gen_palindromes_rec :: String -> [Integer]
gen_palindromes_rec so_far
	= filter (< ub)
	. map (\digit -> [digit] ++ so_far ++ [digit])
	$ "123456789"

palindromes2 :: [Integer]
palindromes2 = gen_palindromes_rec ""

valid_palindromes :: [Integer]
valid_palindromes
	= filter formable
	$ palindromes

main :: IO ()
main = do
	putStrLn . show $ sum valid_palindromes
