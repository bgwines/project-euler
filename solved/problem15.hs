{- Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid? -}

import qualified Data.MemoCombinators as Memo

num_paths :: Integer -> Integer -> Integer
num_paths = Memo.memo2 Memo.integral Memo.integral num_paths'
	where
		num_paths' :: Integer -> Integer -> Integer
		num_paths' n m
			| (m == 0) && (n == 0) = 1
			| (m < 0) || (n < 0) = 0
			| otherwise = 
				num_paths (m - 1) n
				+
				num_paths m (n - 1)

main :: IO ()
main = do
	putStrLn . show $ num_paths 20 20