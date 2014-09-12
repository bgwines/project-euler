{- A composite is a number containing at least two prime factors. For example, 15 = 3 × 5; 9 = 3 × 3; 12 = 2 × 2 × 3.

There are ten composites below thirty containing precisely two, not necessarily distinct, prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26.

How many composite integers, n < 10^8, have precisely two, not necessarily distinct, prime factors? -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Data.MemoCombinators as Memo

import Data.Maybe

import Control.Applicative

ub :: Integer
ub = 10^8

primes :: [Integer]
primes = takeWhile (\p -> (2 * p) < ub) ZMath.primes

-- [(2,all p | p*2 < ub), (3,..),...]
prime_pair_sets :: [(Integer, [Integer])]
prime_pair_sets
	= takeWhile ((/=) [] . snd)
	. map valid_primes
	. List.tails
	$ primes
	where
		valid_primes :: [Integer] -> (Integer, [Integer])
		valid_primes ps = (head ps, rest)
			where
				rest :: [Integer]
				rest = takeWhile (\p -> ((head ps) * p) < ub) ps

sought :: Int
sought
	= length
	. concatMap (\(p, ps) -> map ((*) p) ps)
	$ prime_pair_sets

main :: IO ()
main = do
	putStrLn . show $ ub
	putStrLn . show $ sought
