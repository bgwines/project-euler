{- Using a combination of black square tiles and oblong tiles chosen from: red tiles measuring two units, green tiles measuring three units, and blue tiles measuring four units, it is possible to tile a row measuring five units in length in exactly fifteen different ways. How many ways can a row measuring fifty units in length be tiled?

NOTE: This is related to Problem 116. -}

import qualified Data.MemoCombinators as Memo
import qualified Data.List as List
import Data.Maybe

count_num_tilings :: Integer -> Integer
count_num_tilings = Memo.integral count_num_tilings'
	where
		count_num_tilings' :: Integer -> Integer
		count_num_tilings' 0 = 1
		count_num_tilings' n
			= succ -- empty tiling
			. sum
			. map count_num_tilings -- only recurse on RHS to avoid double-counting tilings
			$ [ n - (i + block_len) -- size of RHS
				| block_len <- [2,3,4]
				, i <- [0..(n - block_len)] ]

main :: IO ()
main = do
	putStrLn . show $ count_num_tilings 50