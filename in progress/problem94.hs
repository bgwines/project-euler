{-
It is easily proved that no equilateral triangle exists with integral length sides and integral area. However, the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).
-}

import Euler
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Ord as Ord
import qualified Data.MemoCombinators as Memo

ub = 1000000000
ub_double = 1000000000.0

squares :: Set.Set Double
squares = Set.fromList $ takeWhile (< ub_double) $ map (^2) [1.0..]

is_square :: Double -> Bool
is_square n = Set.member n squares

-- Heron's formula
area :: Double -> Integer -> Integer -> Integer -> Double
area p a b c = 
    sqrt $ p * (p-a') * (p-b') * (p-c')
    	where
    		a' = fromInteger a
    		b' = fromInteger b
    		c' = fromInteger c

has_int_area :: (Integer, Integer, Integer) -> Bool
has_int_area (a, b, c) =
	is_int $ area p a b c
		where p = (fromInteger (a + b + c)) / 2

triangles :: [(Integer, Integer, Integer)]
triangles = [(a,b,c) | a<-[1..ub], b<-[a+1], c<-[a,b], a+b+c <= (ub `div` 3)]

almost_equilateral :: [(Integer, Integer, Integer)]
almost_equilateral = filter has_int_area triangles

perimeters :: [Integer]
perimeters = map (\(a, b, c) -> a + b + c) almost_equilateral

main = do
    (putStrLn . show) $ sum perimeters