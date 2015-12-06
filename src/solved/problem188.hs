{- The hyperexponentiation or tetration of a number a by a positive int b, denoted by a↑↑b or ba, is recursively defined by:

a↑↑1 = a,
a↑↑(k+1) = a(a↑↑k).

Thus we have e.g. 3↑↑2 = 33 = 27, hence 3↑↑3 = 327 = 7625597484987 and 3↑↑4 is roughly 103.6383346400240996*10^12.

Find the last 8 digits of 1777↑↑1855. -}

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

import Data.Maybe
import Data.Ratio
import Data.Function

import Debug.Trace

import Data.Monoid
import Control.Monad
import Control.Applicative

pow :: Int -> Int -> Int
pow b 0 = 1
pow b 1 = b
pow b_original e_original = pow' b_original e_original 1
	where
		mul :: Int -> Int -> Int
		mul x y = x * y `mod` 10^8

		pow' :: Int -> Int -> Int -> Int
		pow' b e curr_e
			= if curr_e == e
				then b
				else if (curr_e * 2) <= e
					then pow' (b `mul` b) e (curr_e * 2)
					else b `mul` (pow' b_original (e - curr_e) 1)

hexp :: Int -> Int -> Int
hexp b 1 = b
hexp b e = pow b (b `hexp` (e-1))

main :: IO ()
main = do
	putStrLn . show $ 1777 `hexp` 1855
