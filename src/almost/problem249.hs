
{-
Let S = {2, 3, 5, ..., 4999} be the set of prime numbers less than 5000.

Find the number of subsets of S, the sum of whose elements is a prime number. Enter the rightmost 16 digits as your answer.
-}

import qualified Data.Numbers.Primes as P (primes, isPrime)
import qualified Data.MemoCombinators as Memo
import Zora.List (powerset)
import Data.List (sortBy)
import Data.Ord (comparing)

{-
1548136 -- desired sum of S
...
8 |
7 |x x   •   •
6 |x x   x   x
5 |x •   •   •
4 |x x   x   x
3 |x •   •   •
2 |• •   •   •
1 |
0 |
  ------------
   2 3   5   7...4999 -- max usable prime (sUB)
-}

sUB :: Integer
sUB = 20 --5000

s :: [Integer]
s = takeWhile (< sUB) P.primes

-- | Number of ways to form `n` as a summation of primes less than `maxPrime`
numFormations :: Integer -> Integer -> Integer
numFormations 0 _ = 1 -- formation is the empty set
numFormations 1 _ = 0 -- no formations possible
numFormations n maxPrime
    | (n < 0) = 0 -- can't sum to a negative number with only positive numbers
    | otherwise = f n maxPrime
    where
        f = Memo.memo2 Memo.integral Memo.integral numFormations'

-- | Number of ways to form `n` as a summation of primes less than `maxPrime`
numFormations' :: Integer -> Integer -> Integer
numFormations' n maxPrime
    = sum
    . map (\p -> numFormations (n - p) p)
    $ takeWhile (< maxPrime) s

main :: IO ()
main = do
    putStrLn $ "S = takeWhile (< " ++ show sUB ++ ") P.primes"
    putStr "Number of subsets of S that sum to a prime number: "
    let ssum = sum s
    print
        . sum -- TODO: fold with truncating +
        . map (\n -> numFormations n (n + 1)) -- `+ 1` because of noninclusiveness
        . takeWhile (<= ssum) -- `sum s` is for the subset that is `S`; the maximal subset
        $ P.primes
    putStr "Brute force: "
    print $ length . filter P.isPrime . map sum . powerset $ s
