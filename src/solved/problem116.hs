{- A row of five black square tiles is to have a number of its tiles replaced with coloured oblong tiles chosen from red (length two), green (length three), or blue (length four).

If red tiles are chosen there are exactly seven ways this can be done. If green tiles are chosen there are three ways. And if blue tiles are chosen there are two ways.

Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of replacing the black tiles in a row measuring five units in length.

How many different ways can the black tiles in a row measuring fifty units in length be replaced if colours cannot be mixed and at least one coloured tile must be used?

NOTE: This is related to Problem 117. -}

import qualified Data.MemoCombinators as Memo
import qualified Data.List as List
import Data.Maybe

count_num_tilings' :: Integer -> Integer -> Integer
count_num_tilings' = Memo.memo2 Memo.integral Memo.integral count_num_tilings''
	where
		count_num_tilings'' :: Integer -> Integer -> Integer
		count_num_tilings'' block_len 0 = 0
		count_num_tilings'' block_len n
			= sum
			. map succ -- empty tiling on each rhs
			. map (count_num_tilings' block_len) $ rhs_sizes
				where
					-- only recurse on RHS to avoid double-counting tilings
					rhs_sizes :: [Integer]
					rhs_sizes = map (\i -> n - (i + block_len)) starting_indices

					starting_indices :: [Integer]
					starting_indices = [0..(n - block_len)]

count_num_tilings :: Integer -> Integer
count_num_tilings n
	= sum
	. map (flip count_num_tilings' $ n)
	$ [2,3,4]

main :: IO ()
main = do
	putStrLn . show $ count_num_tilings 50
