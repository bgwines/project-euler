{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.List as List

import Data.Maybe

import Control.Applicative

num_solutions :: Integer -> Integer
num_solutions = ZMath.num_divisors_of_n_squared_leq_n

sought :: Integer
sought
	= fromJust
	. List.find ((> 1000) . num_solutions)
	$ [2..]

main :: IO ()
main = do
	putStrLn . show $ sought
