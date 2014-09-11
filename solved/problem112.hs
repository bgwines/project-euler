{- Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.

Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.

We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number; for example, 155349.

Clearly there cannot be any bouncy numbers below one-hundred, but just over half of the numbers below one-thousand (525) are bouncy. In fact, the least number for which the proportion of bouncy numbers first reaches 50% is 538.

Surprisingly, bouncy numbers become more and more common and by the time we reach 21780 the proportion of bouncy numbers is equal to 90%.

Find the least number for which the proportion of bouncy numbers is exactly 99%. -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.List as List

import qualified Data.MemoCombinators as Memo

import Data.Maybe

import Control.Applicative

integer_bounciness_binary :: [Double]
integer_bounciness_binary = map f integer_bounciness
	where
		f :: Bool -> Double
		f e = if e then 1 else 0

		integer_bounciness :: [Bool]
		integer_bounciness = map is_bouncy [1..]

		is_bouncy :: Integer -> Bool
		is_bouncy n = (not . increasing $ n) && (not . decreasing $ n)

		-- can be increasing or decreasing
		directioning :: Integer -> (Char -> Char -> Bool) -> Bool
		directioning n cmp = directioning' ch chs 
			where
				ch :: Char
				chs :: [Char]
				ch:chs = show n

				directioning' :: Char -> [Char] -> Bool
				directioning' _ [] = True
				directioning' last_ch (x:xs)
					= if last_ch `cmp` x
						then False
						else directioning' x xs

		increasing :: Integer -> Bool
		increasing n = directioning n (>)

		decreasing :: Integer -> Bool
		decreasing n = directioning n (<)

prefix_sums_with_indices :: [(Double, Double)]
prefix_sums_with_indices
	= tail
	. scanl (\(a,b) e -> (a+e, b+1)) (0,0)
	$ integer_bounciness_binary

sought :: Integer
sought
	= round
	. snd
	. fromJust
	. List.find (\(e,i) -> e/i >= 0.99)
	$ prefix_sums_with_indices

main :: IO ()
main = do
	putStrLn . show $ sought
