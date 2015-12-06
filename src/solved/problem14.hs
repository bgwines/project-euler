
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.List as List
import qualified Data.MemoCombinators as Memo

import Data.Maybe

collatz :: Int -> Int
collatz n =
	if even n
		then n `div` 2
		else (3 * n) + 1

collatz_seq_len :: Int -> Int
collatz_seq_len = Memo.integral collatz_seq_len'
	where
		collatz_seq_len' :: Int -> Int
		collatz_seq_len' n =
			if n == 1
				then 1
				else ((+) 1) . collatz_seq_len $ collatz n

elem_with_longest_seq :: Int
elem_with_longest_seq
	= List.maximumBy (Ord.comparing collatz_seq_len) [2..1000000]

main :: IO ()
main = do
	putStrLn . show $ elem_with_longest_seq