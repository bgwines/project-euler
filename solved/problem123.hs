{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.List as List

import qualified Data.MemoCombinators as Memo

import Data.Maybe

import Control.Applicative

ub :: Integer
ub = 10^10

f :: Integer -> Integer
f n = ((p - 1)^n + (p + 1)^n) `mod` (p^2)
	where
		p :: Integer
		p = ZMath.primes !! ((fromIntegral n) - 1)

sought :: Integer
sought
	= fst
	. fromJust
	. List.find ((> ub) . snd)
	. ZList.map_keep f
	$ [1,3..]

main :: IO ()
main = do
	putStrLn . show $ sought
