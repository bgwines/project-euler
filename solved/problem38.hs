
import qualified Data.List as List
import qualified Zora.List as ZList

concatenated_triple :: Integer -> Integer -> String
concatenated_triple n m = (show n) ++ (show m) ++ (show (m*n))

pandigital :: String -> Bool
pandigital n = "123456789" == (List.sort n)

forms_pandigital_triple :: Integer -> Integer -> Bool
forms_pandigital_triple n = pandigital . concatenated_triple n

forms_pandigital_multiple :: Integer -> Bool
forms_pandigital_multiple n
	= any pandigital
	. filter    (\m -> (length m) == 9)
	. takeWhile (\m -> (length m) <= 9) 
	. scanl1 (++) 
	. map (show . ((*) n))
	$ [1..n]

pandigital_multiple_formers :: [Integer]
pandigital_multiple_formers = filter forms_pandigital_multiple [1..109739369] -- 109739369 = 987654321 / 9

main :: IO ()
main = do
	putStrLn . show $ pandigital_multiple_formers