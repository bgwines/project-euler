{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.List as List

import Data.Maybe

import Control.Applicative

num_solutions :: Integer -> Integer
num_solutions n
	= ZList.length'
	. takeWhile (<= n)
	. ZMath.divisors
	$ n^2

sought :: Integer
sought
	= fromJust
	. List.find ((> 1000) . num_solutions)
	$ [2..]

main :: IO ()
main = do
	putStrLn . show $ sought
