
import Euler
import Data.List

squares :: [String]
squares = ["01", "04", "09", "16", "25", "36", "49", "64", "81"]

get_concatenations :: [String] -> [String] -> [String]
get_concatenations a b = sort 
    ([x ++ y | x<-a, y<-b] ++ 
     [y ++ x | x<-a, y<-b])

displays_all_squares_wrapped :: [String] -> [String] -> Bool
displays_all_squares_wrapped a b = 
    (Data.List.intersect (get_concatenations a b) squares) == squares

maybe_add_9_or_6 :: [Integer] -> [Integer]
maybe_add_9_or_6 l = 
    case 9 `elem` l of True  -> case 6 `elem` l of True  -> l
                                                   False -> 6 : l
                       False -> case 6 `elem` l of True  -> 9 : l
                                                   False -> l

displays_all_squares :: ([Integer], [Integer]) -> Bool
displays_all_squares (a, b) = displays_all_squares_wrapped (prep a) (prep b)
    where prep = (map show) . maybe_add_9_or_6

cubes :: [[Integer]]
cubes = filter (\l -> (length l) == 6) $ powerset [0..9]

cube_pairs :: [([Integer], [Integer])]
cube_pairs = [(a, b) | a <- cubes, b <- cubes]

valid_pairs :: [([Integer], [Integer])]
valid_pairs = filter displays_all_squares cube_pairs

main = do
	putStrLn $ show $ length valid_pairs