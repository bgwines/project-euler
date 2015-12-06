{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}

import qualified Zora.Math as Math

main :: IO ()
main = do
	-- -1 for off-by-one
	putStrLn . show $ Math.primes !! (10001 - 1)