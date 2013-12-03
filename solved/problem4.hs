{-
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

is_palindrome :: (Eq e) => [e] -> Bool
is_palindrome s
	| length s <= 1 = True
	| otherwise =
		(head s == last s)
		&&
		(is_palindrome $ tail $ init s)

--forms_mult_palindrome_pair :: (Show x, Num x, Show y, Num y) => (x,y) -> Bool
forms_mult_palindrome_pair (a,b) = is_palindrome $ show $ a * b

nums = [100..999]
pairs = [(a,b) | a <- nums, b <- nums]

mult_palindrome_pairs =
	filter forms_mult_palindrome_pair pairs

mults = [a*b | (a,b) <- mult_palindrome_pairs]
m = maximum mults