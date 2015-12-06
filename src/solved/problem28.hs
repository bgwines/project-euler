side_len :: Integer
side_len = 1001

odds :: [Integer]
odds = takeWhile (<= (1 + side_len)) [1,3..]

evens :: [Integer]
evens = takeWhile (<= (1 + side_len)) [2,4..]

odd_squares :: [Integer]
odd_squares = map ((^) 2) odds

even_squares :: [Integer]
even_squares = map ((^) 2) evens

diags :: [Integer]
diags = ul ++ ur ++ dl ++ dr
	where
		ul :: [Integer]
		ul = map succ even_squares

		ur :: [Integer]
		ur = map (\n -> round . succ $ (n - (sqrt n))) even_squares

		dl :: [Integer]
		dl = map (\n -> round . succ $ (n - (sqrt n))) $ tail odd_squares

		dr :: [Integer]
		dr = odd_squares

main :: IO ()
main = do
	putStrLn . show $ sum diags

{-
SIDE_LEN = 1001

odds = [n for n in xrange(1, 1 + SIDE_LEN, 2)]
evens = [n for n in xrange(2, 1 + SIDE_LEN, 2)]
odd_squares = map(lambda n: n**2, odds)
even_squares = map(lambda n: n**2, evens)

UL = map(lambda n: n + 1, even_squares)
UR = map(lambda n: int(n - (n**0.5) + 1), even_squares)
DL = map(lambda n: int(n - (n**0.5) + 1), odd_squares[1:])
DR = odd_squares

diags = UL + UR + DL + DR

print diags, sum(diags)
-}