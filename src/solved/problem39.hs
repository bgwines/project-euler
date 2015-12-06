{-
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000 is the number of solutions maximised?
-}

import qualified Data.Ord as Ord
import qualified Data.List as List

solutions :: Integer -> [(Integer, Integer, Integer)]
solutions p =
	[ (a, b, c)
		| a <- [0..(p - 1)]
		, b <- [a..(div (p - a) 2)]
		, c <- [p - a - b]
		, (a^2) + (b^2) == (c^2) ]

p_with_most_solutions :: Integer
p_with_most_solutions = List.maximumBy (Ord.comparing (length . solutions)) [0..1000]

main :: IO ()
main = do
	putStrLn . show $ p_with_most_solutions