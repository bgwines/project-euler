{-
It is easily proved that no equilateral triangle exists with integral length sides and integral area. However, the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).

------------------------------------
------------------------------------

Pell's equation:
	
	h^2 = a^2 - ((a+-1)^2)/2

	...

	((3a +- 1)/2)^2 - 3h^2 = 1

	=>

	x = (3a +- 1)/2
	y = h
	n = 3
-}

import Euler
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Ord as Ord
import qualified Data.MemoCombinators as Memo

-- initial solution to Pell's equation for n = 3
(x0, y0) = (2, 1)

n = 3

next :: (Integer, Integer) -> (Integer, Integer)
next (x, y) = (x', y')
	where
		x' = x0*x + n*y0*y
		y' = x0*y +   y0*x

to_triangle :: (Integer -> Integer) -> (Integer -> Integer) -> (Integer, Integer) -> (Integer, Integer, Integer)
to_triangle op1 op2 (x, y) =
	let
		z = toInteger . round $ (fromInteger . op2 $ 2*x) / 3
	in
		(z, z, op1 z)

triangles :: (Integer -> Integer) -> (Integer -> Integer) -> [(Integer, Integer, Integer)]
triangles op1 op2 =
	takeWhile small_perimeter
		$ map (to_triangle op1 op2)
			$ filter valid solutions
	where
		solutions = iterate next (x0, y0)
		small_perimeter (a, b, c) = a + b + c < 1000000000
		valid (x, y) =
			((y * (op2 . op2 $ x)) `mod` 3) == 0 --area is integer
			&&
			((op2 $ 2 * x) `mod` 3) == 0

main = do
    (putStrLn . show) $ f (tail $ triangles succ pred) + f (triangles pred succ) --tail to get rid of (1, 1, 2)
		where f = sum . map (\(a, b, c) -> a + b + c)
