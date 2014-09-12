{- The binomial coefficients nCk can be arranged in triangular form, Pascal's triangle, like this:

1	
1		1	
1		2		1	
1		3		3		1	
1		4		6		4		1	
1		5		10		10		5		1	
1		6		15		20		15		6		1	
1		7		21		35		35		21		7		1
.........
It can be seen that the first eight rows of Pascal's triangle contain twelve distinct numbers: 1, 2, 3, 4, 5, 6, 7, 10, 15, 20, 21 and 35.

A positive integer n is called squarefree if no square of a prime divides n. Of the twelve distinct numbers in the first eight rows of Pascal's triangle, all except 4 and 20 are squarefree. The sum of the distinct squarefree numbers in the first eight rows is 105.

Find the sum of the distinct squarefree numbers in the first 51 rows of Pascal's triangle. -}

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

pascal_triangle :: [[Integer]]
pascal_triangle = take 51 $ pascal_triangle' [1]

pascal_triangle' :: [Integer] -> [[Integer]]
pascal_triangle' last_row
	= (:) last_row $ pascal_triangle' curr_row
	where
		curr_row :: [Integer]
		curr_row = [1] ++ (zipWith (+) last_row (tail last_row)) ++ [1]

squarefree :: Integer -> Bool
squarefree
	= all (< 2)
	. map snd
	. ZList.elem_counts
	. ZMath.factor

sought :: Integer
sought
	= sum
	. filter squarefree
	. ZList.uniqueify
	. concat
	$ pascal_triangle

main :: IO ()
main = do
	putStrLn . show $ sought
