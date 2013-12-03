
import Data.Ord
import Data.List

b :: Bool -> Integer 
b True = 1
b False = 0

divides_both :: Integer -> Integer -> Integer -> Bool
divides_both d n m = (n `mod` d == 0) && (m `mod` d == 0)

coprime :: Integer -> Integer -> Bool
coprime n m = 0 == n_shared_divisors
	where n_shared_divisors = 
		sum $ map b [divides_both d n m | d <- less]
			where less = [2..(min n m)]

euler_phi_rec :: Integer -> Integer -> Integer -> Integer
euler_phi_rec n m count
	| (n == m) = count
	| (coprime n m) = euler_phi_rec n (m+1) (count+1)
	| otherwise		= euler_phi_rec n (m+1) count

euler_phi :: Integer -> Integer
euler_phi n = euler_phi_rec n 1 0

f :: Integer -> Float
f n = (fromIntegral n) / (fromIntegral (fast_euler_phi n))

----------

fast_euler_phi :: Integer -> [Integer] -> Integer
fast_euler_phi n 
	| n == 2 = 1
	| (even n) = case (even half) of
		True -> 2 * (fast_euler_phi half)
		False -> 1 * (fast_euler_phi half)
	| (odd  n) = euler_phi n
		where half = floor $ fromIntegral n/2 --already an int because even

totients = [fast_euler_phi n | n <- [1..10]]

----------

ns = map fromIntegral [2..1000]

l = [(n, f n) | n <- ns]

sought = maximumBy (comparing (\(n, fn) -> fn)) l