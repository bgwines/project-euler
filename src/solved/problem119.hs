{- The number 512 is interesting because it is equal to the sum of its digits raised to some power: 5 + 1 + 2 = 8, and 83 = 512. Another example of a number with this property is 614656 = 284.

We shall define an to be the nth term of this sequence and insist that a number must contain at least two digits to have a sum.

You are given that a2 = 512 and a10 = 614656.

Find a30. -}

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

sum_of_digits :: Integer -> Integer
sum_of_digits
	= fromIntegral
	. sum
	. map Char.digitToInt
	. show

sought :: Integer
sought
	= last
	. take 30
	. List.sort
	. filter (> 10)
	. concatMap (\l -> filter (\n -> (sum_of_digits n) == (head l)) l)
	. map (\x -> takeWhile (< 10^20) . map (x^) $ [1..25])
	$ [2..1000]

main :: IO ()
main = do
	putStrLn . show $ sought
