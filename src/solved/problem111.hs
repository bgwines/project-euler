{- Considering 10-digit primes containing repeated digits it is clear that they cannot all be the same: 1111 is divisible by 11, 2222 is divisible by 22, and so on. But there are nine 10-digit primes containing three ones:

1117, 1151, 1171, 1181, 1511, 1811, 2111, 10111, 8111

We shall say that M(n, d) represents the maximum number of repeated digits for an n-digit prime where d is the repeated digit, N(n, d) represents the number of such primes, and S(n, d) represents the sum of these primes.

So M(10, 1) = 3 is the maximum number of repeated digits for a 10-digit prime where one is the repeated digit, there are N(10, 1) = 9 such primes, and the sum of these primes is S(10, 1) = 22275. It turns out that for d = 0, it is only possible to have M(10, 0) = 2 repeated digits, but there are N(10, 0) = 13 such cases.

In the same way we obtain the following results for 10-digit primes.

Digit, d	M(10, d)	N(10, d)	S(10, d)
0	2	13	67061
1	3	9	22275
2	3	1	2221
3	3	12	1062110
10	3	2	8888
5	3	1	5557
6	3	1	6661
7	3	9	57863
8	3	1	8887
9	3	7	108073
For d = 0 to 9, the sum of all S(10, d) is 273700.

Find the sum of all S(10, d). -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Data.Numbers.Primes as Primes

import qualified Data.MemoCombinators as Memo

import Data.Maybe
import Data.Ratio
import Data.Function

import Control.Applicative

change_digit :: Int -> Int -> Int -> Int
change_digit n i d
	= read
	. ZList.pair_op (++)
	. ZList.map_snd (\s -> (Char.intToDigit d) : (tail s))
	. splitAt i
	$ show n

all_digit_changes :: (Int, Int) -> [(Int, Int)]
all_digit_changes (n, src_digit)
	= filter ((==) 10 . length . show . fst)
	$ [ (change_digit n i d, src_digit)
		| i <- filter valid_i [0..9]
		, d <- valid_ds ]
	where
		valid_i :: Int -> Bool
		valid_i i
			= (==) src_digit
			. Char.digitToInt
			$ (show n) !! i

		valid_ds :: [Int]
		valid_ds = filter ((/=) src_digit) [0..9]

top_level :: [[(Int, Int)]]
top_level
	= zeroes : (map fix_last_digit top_level')
	where
		fix_last_digit :: (Int, Int) -> [(Int, Int)]
		fix_last_digit (n, d) =
			if (even d) || (d == 5)
				then map (\d' -> (change_digit n 9 d', d)) [1,3,7,9]
				else [(n,d)]


		top_level' :: [(Int, Int)]
		top_level'
			= map
				( (\n -> (n, Char.digitToInt . head . show $ n))
				. read
				. replicate 10
				. Char.intToDigit )
			$ [1..9]

		zeroes :: [(Int, Int)]
		zeroes = zip zeroes' (repeat 0)

		zeroes' :: [Int]
		zeroes' = (\a b -> 10^(10-1) * a + b) <$> [1..9] <*> [1,3,7,9]

highest_level_with_primes :: [(Int, Int)] -> [Int]
highest_level_with_primes
	= filter (ZMath.prime_miller_rabin . fromIntegral)
	. map fst
	. fromJust
	. List.find (any (ZMath.prime_miller_rabin . fromIntegral . fst))
	. iterate f
	where
		f :: [(Int, Int)] -> [(Int, Int)]
		f = ZList.uniqueify . concatMap all_digit_changes

sought :: Int
sought
	= sum
	. map (sum . highest_level_with_primes)
	$ top_level

main :: IO ()
main = do
	putStrLn . show $ sought
