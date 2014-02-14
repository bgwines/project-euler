
import Euler
import qualified Data.List as List

squares :: [String]
squares = ["01", "04", "09", "16", "25", "36", "49", "64", "81"]

get_concatenations :: [String] -> [String] -> [String]
get_concatenations a b =
	[x ++ y | x<-a, y<-b] ++ 
    [y ++ x | x<-a, y<-b]

displays_all_squares' :: [String] -> [String] -> Bool
displays_all_squares' a b = and $ map formed squares
	where
		formed sq = sq `elem` (get_concatenations a b)

maybe_add_9_or_6 :: [Integer] -> [Integer]
maybe_add_9_or_6 l = 
    case 9 `elem` l of True  -> case 6 `elem` l of True  -> l
                                                   False -> 6 : l
                       False -> case 6 `elem` l of True  -> 9 : l
                                                   False -> l

displays_all_squares :: ([Integer], [Integer]) -> Bool
displays_all_squares (a, b) = displays_all_squares' a' b'
    where
    	a' = prep a
    	b' = prep b
    	prep = (map show) . maybe_add_9_or_6

valid_pairs :: [([Integer], [Integer])]
valid_pairs = filter displays_all_squares cube_pairs
	where
		cubes = filter (\l -> (length l) == 6) $ powerset [0..9]
		cube_pairs = [(a, b) | a <- cubes, b <- cubes, a < b]

main = do
	(putStrLn . show) $ (length valid_pairs)