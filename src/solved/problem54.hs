
is_palindrome n = (show n) == (reverse $ show n)

addrev n = n + (read $ reverse $ show n)

is_lychrel_rec _ 50 = False
is_lychrel_rec n it
	| (is_palindrome n) = True
	| otherwise = is_lychrel_rec (addrev n) (it+1)

is_lychrel n =
	is_lychrel_rec (addrev n) 1

booleanify True = 0
booleanify False = 1

sought = sum $ map booleanify $ map is_lychrel [0..(10000-1)]