
{- We shall define a square lamina to be a square outline with a square "hole" so that the shape possesses vertical and horizontal symmetry.

Given eight tiles it is possible to form a lamina in only one way: 3x3 square with a 1x1 hole in the middle. However, using thirty-two tiles it is possible to form two distinct laminae.


If t represents the number of tiles used, we shall say that t = 8 is type L(1) and t = 32 is type L(2).

Let N(n) be the number of t ≤ 1000000 such that t is type L(n); for example, N(15) = 832.

What is ∑ N(n) for 1 ≤ n ≤ 10?

--------------------------------

Solution co-written with Julie Tibshirani
-}

-----------------------------------------------
-- Solution co-written with Julie Tibshirani --
-----------------------------------------------

import qualified Data.Ord as Ord
import qualified Data.List as List

import Data.Maybe

t_ub :: Integer
t_ub = 1000000

-- list of outlines represented as number of tiles
valid_ts :: [Integer]
valid_ts = concatMap f squares
	where
		f :: Integer -> [Integer]
		f outer
			= map ((-) outer)
			. filter (same_parity outer)
			$ inners'
			where
				inners' :: [Integer]
				inners' = map (^2) [min_inner_diameter..(sqrt_outer)]

				sqrt_outer :: Integer
				sqrt_outer = integer_sqrt outer

				min_inner_diameter :: Integer
				min_inner_diameter = if outer < t_ub
					then 1
					else integer_sqrt $ outer - t_ub

		same_parity :: Integer -> Integer -> Bool
		same_parity s1 s2 = even $ s1 - s2

		squares :: [Integer]
		squares = map (^2) [1..outer_ub]

		outer_ub :: Integer
		outer_ub = (t_ub `div` 4) + 1

integer_sqrt :: Integer -> Integer
integer_sqrt = toInteger . ceiling . sqrt . fromInteger

-- contains duplicates
ltype_instances :: [Int]
ltype_instances
	= filter (<= 10)
	. map length
	. List.group
	. List.sort
	$ valid_ts

n_value_sums :: Int
n_value_sums
	= sum
	. map length
	. List.group
	. List.sort
	$ ltype_instances

main :: IO ()
main = do
	putStrLn . show $ n_value_sums
