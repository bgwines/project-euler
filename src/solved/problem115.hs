{- NOTE: This is a more difficult version of Problem 114.

A row measuring n units in length has red blocks with a minimum length of m units placed on it, such that any two red blocks (which are allowed to be different lengths) are separated by at least one black square.

Let the fill-count function, F(m, n), represent the number of ways that a row can be filled.

For example, F(3, 29) = 673135 and F(3, 30) = 1089155.

That is, for m = 3, it can be seen that n = 30 is the smallest value for which the fill-count function first exceeds one million.

In the same way, for m = 10, it can be verified that F(10, 56) = 880711 and F(10, 57) = 1148904, so n = 57 is the least value for which the fill-count function first exceeds one million.

For m = 50, find the least value of n for which the fill-count function first exceeds one million. -}

import qualified Data.MemoCombinators as Memo
import qualified Data.List as List
import Data.Maybe

m :: Integer
m = 50

count_num_tilings :: Integer -> Integer
count_num_tilings = Memo.integral count_num_tilings'
	where
		count_num_tilings' :: Integer -> Integer
		count_num_tilings' 0 = 1
		count_num_tilings' n
			= succ -- empty tiling
			. sum
			. map count_num_tilings -- only recurse on RHS to avoid double-counting tilings
			$ [ n - (i + red_block_len) - 1 -- size of RHS - single black tile
				| red_block_len <- [m..n]
				, i <- [0..(n - red_block_len)] ]

sought :: Integer
sought
	= fromJust
	. List.find (\n -> count_num_tilings n > 1000000)
	$ [m..] 

main :: IO ()
main = do
	putStrLn . show $ sought
