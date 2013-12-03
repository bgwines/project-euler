
import Data.List
import List

get_concatenated_triple :: Integer -> Integer -> String
get_concatenated_triple n m = (show n) ++ (show m) ++ (show (m*n))

is_pandigital :: String -> Bool
is_pandigital n = "123456789" == (sort n)

forms_pandigital_triple :: Integer -> Integer -> Bool
forms_pandigital_triple n m = is_pandigital $ get_concatenated_triple n m

forms_pandigital_multiple :: Integer -> Bool
forms_pandigital_multiple n = 
	or $ map is_pandigital
		$ filter (\m -> (length m) == 9)
			$ takeWhile (\m -> (length m) <= 9) 
				$ scanl1 (++) 
					$ map (show . (n *)) [1..n]

foldl1_while :: (a -> a -> a) -> (a -> Bool) -> [a] -> a
foldl1_while f g l = last $ takeWhile g $ scanl1 f l

sought = filter forms_pandigital_multiple [1..109739369] -- 109739369 = 987654321 / 9