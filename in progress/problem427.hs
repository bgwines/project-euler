{- A sequence of integers S = {si} is called an n-sequence if it has n elements and each element si satisfies 1 ≤ si ≤ n. Thus there are nn distinct n-sequences in total. For example, the sequence S = {1, 7, 7, 10, 7, 7, 7, 2, 3, 7} is a 10-sequence.

For any sequence S, let L(S) be the length of the longest contiguous subsequence of S with the same value. For example, for the given sequence S above, L(S) = 3, because of the three consecutive 7's.

Let f(n) = ∑ L(S) for all n-sequences S.

For example, f(3) = 47, f(7) = 1403689 and f(11) = 481496897121.

Find f(7 700 000) mod 1 000 000 009. -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Data.Numbers.Primes as Primes

import qualified Data.MemoCombinators as Memo

import Debug.Trace

import Data.Maybe
import Data.Ratio
import Data.Function

import Control.Applicative

p :: Integer
p = 1000000009

----------------------------------------------------------------------
----------------------------------------------------------------------
-- O(n^3) (correct)
-- each recursive call with `start` and `run_length` corresponds
-- to that being the *first* contiguous subsequence of that length, 
-- where that length is the maximum length to occur (call this 
-- property property "(**)".
num_sequences :: Integer -> Integer -> Integer -> Integer
num_sequences = Memo.memo3 Memo.integral Memo.integral Memo.integral num_sequences'

num_sequences' :: Integer -> Integer -> Integer -> Integer
num_sequences' 0 _ _ = 1
num_sequences' _ 0 _ = 0
num_sequences' seq_length max_run_length n
	= (* n) $ sum
		[ (*)
			(scale n $ num_sequences start (run_length - 1) n) -- (**)
			(scale n $ num_sequences (seq_length - start - run_length) run_length n)
		| start <- [0..(seq_length - 1)]
		, run_length <- [1..(min (seq_length - start) max_run_length)] ]

scale :: Integer -> Integer -> Integer
scale n 1 = 1 -- there are no digits to avoid
scale n k = (k * (n - 1)) `div` n

f :: Integer -> Integer
f n = sum
	[ k * ((num_sequences n k n) - (num_sequences n (k-1) n))
	| k <- [1..n] ]

sought :: Integer
sought = f 6

main :: IO ()
main = do
	putStrLn . show $ sought
