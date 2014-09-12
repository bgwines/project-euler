{- Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0,
where each “_” is a single digit. -}

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

candidates :: [Integer]
candidates
	= map ((*) 10)
	. filter last_digits_will_match_pattern
	$ [a..b]
	where
		-- 1_2_3_4_5_6_7_8_9_0 ends in a '0', so sqrt must also end in 0
		-- similarly, sqrt must only end in one '0', square would end in 0000.

		last_digits_will_match_pattern :: Integer -> Bool
		last_digits_will_match_pattern x
			= (ch == '3') || (ch == '7')
			where
				ch :: Char
				ch = last . show $ x

		a :: Integer
		a = (floor . sqrt $ 1020304050607080900) `div` 10

		b :: Integer
		b = (ceiling . sqrt $ 1929394959697989990 ) `div` 10

matches_pattern :: Integer -> Bool
matches_pattern
	= (==) "1234567890"
	. ZList.passing_index_elems even
	. show

sought :: Integer
sought
	= fromJust
	. List.find (matches_pattern . (^2))
	$ candidates

main :: IO ()
main = do
	putStrLn . show $ sought
