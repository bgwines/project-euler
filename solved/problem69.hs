
import List
import Data.Ord
import Data.List

f :: Integer -> Float
f n = (fromIntegral n) / (fromIntegral (fast_euler_phi n))

----------

fast_euler_phi :: Integer -> Integer
fast_euler_phi n 
	| n == 2 = 1
	| (even n) = case (even half) of
		True  -> 2 * (fast_euler_phi half)
		False -> 1 * (fast_euler_phi half)
	| (odd  n) = euler_phi n
		where half = div n 2

totients = [fast_euler_phi n | n <- [1..10]]

----------

ns = map fromIntegral [2..1000]

l = [(n, f n) | n <- ns]

sought = maximumBy (comparing (\(n, fn) -> fn)) l