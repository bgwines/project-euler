{- Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number; for example, 155349.

As n increases, the proportion of bouncy numbers below n increases such that there are only 12951 numbers below one-million that are not bouncy and only 277032 non-bouncy numbers below 1010.

How many numbers below a googol (10100) are not bouncy? -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.List as List

import qualified Data.MemoCombinators as Memo

import Data.Maybe

import Control.Applicative

-- k: num_digits, n: last digit
num_increasing :: Integer -> Integer -> Integer
num_increasing = Memo.memo2 Memo.integral Memo.integral num_increasing'
num_increasing' _ 0 = 0
num_increasing' 1 n = 1
num_increasing' k n
	= sum
	. map (num_increasing (k - 1))
	$ [1..n]

-- k: num_digits, n: last digit
num_decreasing :: Integer -> Integer -> Integer
num_decreasing = Memo.memo2 Memo.integral Memo.integral num_decreasing'
num_decreasing' 1 0 = 0
num_decreasing' 1 n = 1
num_decreasing' k n
	= sum
	. map (num_decreasing (k - 1))
	$ [n..9]

num_nonbouncy_with_k_digits :: Integer -> Integer
num_nonbouncy_with_k_digits k
	= (flip (-)) 9
	. sum
	$
		[ sum . map (num_increasing k) $ [0..9]
		, sum . map (num_decreasing k) $ [0..9] ]

num_nonbouncy_with_at_most_k_digits :: Integer -> Integer
num_nonbouncy_with_at_most_k_digits k
	= sum
	. map num_nonbouncy_with_k_digits
	$ [1..k]

sought :: Integer
sought = num_nonbouncy_with_at_most_k_digits 100

main :: IO ()
main = do
	putStrLn . show $ sought
