{-
Comparing two numbers written in index form like 211 and 37 is not difficult, as any calculator would confirm that 211 = 2048 < 37 = 2187.

However, confirming that 632382518061 > 519432525806 would be much more difficult, as both numbers contain over three million digits.

Using base_exp.txt (right click and 'Save Link/Target As...'), a 22K text file containing one thousand lines with a base/exponent pair on each line, determine which line number has the greatest numerical value.

NOTE: The first two lines in the file represent the numbers in the example given above.
-}

import Euler
import qualified Data.List
import qualified Data.List.Split
import qualified Data.Ord

eval :: String -> Int
eval s = base ^ exp
	where
		l = map read $ Data.List.Split.splitOn "," s :: [Int]
		(base, exp) = (head l, last l)

find_index_of_max :: [(Integer, String)] -> Integer
find_index_of_max l = fst $ Data.List.maximumBy (Data.Ord.comparing (eval . snd)) l

main = do
	contents <- getContents
	(putStrLn . show . find_index_of_max . indexify . lines) contents
	where
		indexify = zipWith (\a b -> (a, b)) [1..]