{- A Hamming number is a positive number which has no prime factor larger than 5.
So the first few Hamming numbers are 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15.
There are 1105 Hamming numbers not exceeding 10^8.

We will call a positive number a generalised Hamming number of type n, if it has no prime factor larger than n.
Hence the Hamming numbers are the generalised Hamming numbers of type 5.

How many generalised Hamming numbers of type 100 are there which don't exceed 10^9? -}

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

(+++) :: [a] -> [a] -> [a] -> [a]
(+++) a b c = a ++ b ++ c

ub :: Integer
ub = 10^9

primes :: [Integer]
primes = takeWhile (<= 100) ZMath.primes

prime_lists :: Integer -> [Integer] -> [Integer]
prime_lists _ [] = []
prime_lists so_far' ps
	= if so_far > ub
		then []
		else
			(:) so_far
			. concatMap (prime_lists so_far)
			. List.tails
			$ ps
	where
		so_far :: Integer
		so_far = so_far' * (head ps)

hammings :: [Integer]
hammings
	= (:) 1
	. concatMap (prime_lists 1)
	. List.tails
	$ primes

main :: IO ()
main = do
	putStrLn . show $ length hammings
