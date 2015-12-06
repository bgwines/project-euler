{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
-}

import Zora.Math

n :: Integer
n = 600851475143

prime_factors :: [Integer]
prime_factors = filter (\p -> (n `mod` p) == 0) smallprimes
	where
		smallprimes :: [Integer]
		smallprimes = takeWhile (< ((div n 2) + 1)) primes

main :: IO ()
main = do
  putStrLn . show $ maximum prime_factors