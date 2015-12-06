{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

import Data.List
import Data.Maybe

triplets :: [(Integer, Integer, Integer)]
triplets = [(a,b,c) | 
		a <- [1..998], 
		b <- [(a+1)..999], 
		c <- [(b+1)..1000], 
		a + b + c == 1000]

is_pythag_triplet :: (Integer, Integer, Integer) -> Bool
is_pythag_triplet (a,b,c) = (a^2 + b^2) == c^2

pythag_triplet :: (Integer, Integer, Integer)
pythag_triplet = fromJust . find is_pythag_triplet $ triplets

pythag_triplet_prod :: Integer
pythag_triplet_prod = 
	((\(a,b,c) -> a) pythag_triplet) *
	((\(a,b,c) -> b) pythag_triplet) *
	((\(a,b,c) -> c) pythag_triplet)

main :: IO ()
main = do
	putStrLn . show $ pythag_triplet_prod