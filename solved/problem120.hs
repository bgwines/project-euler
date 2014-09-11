{- Let r be the remainder when (a−1)n + (a+1)n is divided by a2.

For example, if a = 7 and n = 3, then r = 42: 63 + 83 = 728 ≡ 42 mod 49. And as n varies, so too will r, but for a = 7 it turns out that rmax = 42.

For 3 ≤ a ≤ 1000, find ∑ rmax. -}

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

r_max :: Integer -> Integer
r_max a = maximum [(2 * n * a) `mod` (a^2) | n <- [1..(2*a)]]

sought :: Integer
sought
	= sum
	. map r_max
	$ [3..1000]

main :: IO ()
main = do
	putStrLn . show $ sought
