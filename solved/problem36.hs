
import qualified Zora.List as ZList
import qualified Data.Char as Char
import qualified Numeric

represent_in_base :: Integer -> Integer -> String
represent_in_base b n
	= Numeric.showIntAtBase b Char.intToDigit n ""

both_bases_palindromic :: Integer -> Bool
both_bases_palindromic n = 
	(ZList.is_palindrome $ show n) &&
	(ZList.is_palindrome $ represent_in_base 2 n)

sum_of_valid_nums :: Integer
sum_of_valid_nums
	= sum
	. filter both_bases_palindromic
	$ [1..1000000]

main :: IO ()
main = do
	putStrLn . show $ sum_of_valid_nums