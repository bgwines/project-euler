
import Data.Char
import Numeric
import List

represent_in_base :: Integer -> Integer -> String
represent_in_base b n = showIntAtBase b intToDigit n ""

both_bases_palindromic n = 
	(is_palindrome $ show n) &&
	(is_palindrome $ represent_in_base 2 n)

l = filter both_bases_palindromic [1..1000000]
sought = sum l