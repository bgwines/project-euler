{-
The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.

How many n-digit positive integers exist which are also an nth power?
-}

import qualified Zora.List as ZList

num_powers_of_length_power :: Integer -> Integer
num_powers_of_length_power n
	= toInteger
	. length
	. takeWhile ((==) EQ)
	. map (ZList.pair_op compare)
	$ lengths_and_powers
	where
		lengths_and_powers :: [(Integer, Integer)]
		lengths_and_powers = map ((ZList.map_fst f) . duplicate) [1..50]

		duplicate :: a -> (a, a)
		duplicate x = (x, x)

		f :: Integer -> Integer
		f = toInteger . length . show . (n ^)

sought :: Integer
sought = sum . map num_powers_of_length_power $ [1..30]

main :: IO ()
main = do
	putStrLn . show $ sought