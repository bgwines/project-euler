{- A positive fraction whose numerator is less than its denominator is called a proper fraction.
For any denominator, d, there will be d−1 proper fractions; for example, with d = 12:
1/12 , 2/12 , 3/12 , 4/12 , 5/12 , 6/12 , 7/12 , 8/12 , 9/12 , 10/12 , 11/12 .

We shall call a fraction that cannot be cancelled down a resilient fraction.
Furthermore we shall define the resilience of a denominator, R(d), to be the ratio of its proper fractions that are resilient; for example, R(12) = 4/11 .
In fact, d = 12 is the smallest denominator having a resilience R(d) < 4/10 .

Find the smallest denominator d, having a resilience R(d) < 15499/94744 . -}

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

{- OK, so this is pretty cool. If you look at the "running bests",
   i.e. numbers that break the "best so far" record for resilience
   if we consider numbers in an increasing fashion, they follow a pattern:

   [2,3,4,6,12,18,24,30,60,90,120,150,180,210,420,630,840,1050,1260,1470,1680,1890,2100,2310,4620,6930,9240,11550,13860,16170,18480,20790,23100,25410,27720,30030,60060,90090,120120,150150,180180,210210,240240,270270,300300,330330,360360,390390,420420,450450,480480,510510,1021020,1531530...]

   λ zipWith (-) (tail it) it
   [1,1,2,6,6,6,6,30,30,30,30,30,30,210,210,210,210,210,210,210,210,210,210,2310,2310,2310,2310,2310,2310,2310,2310,2310,2310,2310,2310,30030,30030,30030,30030]

   λ map (\y -> (head y, length y)) . List.group $ it
   [(1,2),(2,1),(6,4),(30,6),(210,10),(2310,12),(30030,4)]

   λ let ns = map fst it
   λ let lengths = map snd it

   λ zipWith (/) (tail ns) ns
   [2.0,3.0,5.0,7.0,11.0,13.0]

   λ lengths
   [2,1,4,6,10,12,16]
   λ take 7 . map pred $ ZMath.primes
   [1,2,4,6,10,12,16]
-}

ratio :: Double
ratio = 15499 / 94744

resilience :: Integer -> Double
resilience n
	= (fromIntegral $ ZMath.euler_phi n) / (fromIntegral $ pred n)

running_bests_sequence :: [Integer]
running_bests_sequence = scanl1 (+) replicated_diffs
	where
		replicated_diffs :: [Integer]
		replicated_diffs
			= (++) [2,1,1]
			. tail
			. concat
			$ zipWith replicate lengths diffs

		lengths :: [Int]
		lengths = map (fromIntegral . pred) . tail $ ZMath.primes

		diffs :: [Integer]
		diffs = scanl1 (*) ZMath.primes

sought :: Integer
sought
	= fromJust
	. List.find ((< ratio) . resilience)
	$ running_bests_sequence

main :: IO ()
main = do
	putStrLn . show $ sought
