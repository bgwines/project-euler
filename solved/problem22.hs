
{-
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
-}

import Data.Char

score_name :: (Int, String) -> Int
score_name (i, name) = (*) i $ sum . map f $ name
	where
		f :: Char -> Int
		f ch = succ $ (ord ch) - (ord 'A')

sum_name_scores :: String -> Int
sum_name_scores file = sum . map score_name . zip [1..] $ names
	where
		names :: [String]
		names = concatMap words . lines $ file

main :: IO ()
main = do
	file <- readFile "names.txt"
	putStrLn . show $ sum_name_scores file
