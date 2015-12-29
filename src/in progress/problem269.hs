{- It can be verified that there are 23 positive integers less than 1000 that are divisible by at least four distinct primes less than 100.

Find how many positive integers less than 10^16 are divisible by at least four distinct primes less than 100. -}

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

ub :: Integer
ub = 1000

primes :: [Integer]
primes = takeWhile (< 100) ZMath.primes

prime_sets :: [[Integer]]
prime_sets
	= takeWhile (any (< ub))
	. map (map product . ZList.subsets_of_size primes)
	$ [4..]

main :: IO ()
main = do
	putStrLn . show $ prime_sets
