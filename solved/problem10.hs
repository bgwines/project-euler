{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

import qualified Zora.Math as Math

primes_under :: [Integer]
primes_under = takeWhile (< 2000000) Math.primes

main :: IO ()
main = do
  putStrLn . show $ sum primes_under