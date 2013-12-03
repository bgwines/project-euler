{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

import Prime

primes_under :: [Integer]
primes_under = takeWhile (\x -> x < 2000000) primes

main = do
  (putStrLn . show) sum primes_under