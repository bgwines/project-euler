{-
In all there are 10400 strings of length 3 for which exactly one character comes lexicographically after its neighbour to the left.



26 C n strings total

each length n, all chars distinct

order each one

n places for offending char to go
-}

choose :: Integer -> Integer -> Integer
choose n k = (fact n) `div` (product . map fact $ [k, n-k])
	where
		fact :: Integer -> Integer
		fact n = product [1..n]