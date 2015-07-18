
{-
Let S = {2, 3, 5, ..., 4999} be the set of prime numbers less than 5000.

Find the number of subsets of S, the sum of whose elements is a prime number. Enter the rightmost 16 digits as your answer.
-}

import qualified Zora.List as ZL (powerset, ($$))
import qualified Data.Numbers.Primes as P (primes, isPrime)
import qualified Data.MemoCombinators as Memo

{-
...
8 |
7 |x x • • • • •
6 |x x x x x x x
5 |x • • • • • •
4 |x x x x x x x
3 |x • • • • • •
2 |• • • • • • •
  --------------
   2 3 5 7 11 13...4999
-}

sUB :: Integer
sUB = 60 --5000

s :: [Integer]
s = takeWhile (< sUB) P.primes

allPrimes :: [Integer]
allPrimes = takeWhile (<= (sum s)) P.primes

numFormations :: Integer -> Integer -> Integer
numFormations n maxPrime
    | n <= 1 = 0
    | otherwise = (Memo.memo2 Memo.integral Memo.integral numFormations') n maxPrime

numFormations' :: Integer -> Integer -> Integer
numFormations' n maxPrime -- "maxPrime" is UB on primes; not necessarily prime itself
    | (n <= sUB)
        && (n <= maxPrime)
        && (P.isPrime n)
        = 1 + numFormations n (maxPrime - 1)
    | otherwise
        = sum
        . map (\p -> numFormations
                        (n - p)
                        (min (p - 1) (n - p)) ) -- p-1 so that we don't re-use components of a path
        $ takeWhile (< maxPrime) s

-- mod 10^18
numPrimeSubsets :: Integer
numPrimeSubsets
    = foldl1 (\a b -> (a + b) `mod` (10^18))
    . map (\n -> numFormations n n)
    $ allPrimes

main :: IO ()
main = do
    -- TODO: fold with truncating +
    print sUB
    putStrLn $ take 16 . show $ numPrimeSubsets
