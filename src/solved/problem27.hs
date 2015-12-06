{-
Euler discovered the remarkable quadratic formula:

n^2 + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

The incredible formula  n^2 − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

n^2 + an + b, where |a| < 1000 and |b| < 1000

where |n| is the modulus/absolute value of n
e.g. |11| = 11 and |−4| = 4
Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
-}

import qualified Data.List as List
import qualified Data.Ord as Ord

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import Control.Applicative

num_primes_produced :: (Integer -> Integer) -> Int
num_primes_produced f =
	length . takeWhile (ZMath.prime . f) $ [0..]

k :: Integer
k = 999

as :: [Integer]
as = [-k..k]

positive_small_primes :: [Integer]
positive_small_primes = takeWhile (<= k) ZMath.primes

quadratics :: [(Integer -> Integer, (Integer, Integer))]
quadratics = f <$> as <*> positive_small_primes
	where
		f :: Integer -> Integer -> (Integer -> Integer, (Integer, Integer))
		f a b = ((\n -> n^2 + (a * n) + b), (a, b))

fs_with_results :: [(Int, (Integer, Integer))]
fs_with_results = map (ZList.map_fst (($) num_primes_produced))quadratics

max_with_index :: ((Int, (Integer, Integer)), Integer)
max_with_index = ZList.maximum_with_index fs_with_results

ab :: (Integer, Integer)
ab = snd . fst $ max_with_index

main :: IO ()
main = do
	putStrLn . show $ fst ab * snd ab