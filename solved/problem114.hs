{-A row measuring n units in length has red blocks with a minimum length of m units placed on it, such that any two red blocks (which are allowed to be different lengths) are separated by at least one black square.

Let the fill-count function, F(m, n), represent the number of ways that a row can be filled.

For example, F(3, 29) = 673135 and F(3, 30) = 1089155.

Find F(3, 50)
-}

import qualified Data.MemoCombinators as Memo

m :: Integer
m = 3

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

main :: IO ()
main = do
	putStrLn . show $ count_num_tilings 50
