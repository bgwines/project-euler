{- A bag contains one red disc and one blue disc. In a game of chance a player takes a disc at random and its colour is noted. After each turn the disc is returned to the bag, an extra red disc is added, and another disc is taken at random.

The player pays £1 to play and wins if they have taken more blue discs than red discs at the end of the game.

If the game is played for four turns, the probability of a player winning is exactly 11/120, and so the maximum prize fund the banker should allocate for winning in this game would be £10 before they would expect to incur a loss. Note that any payout will be a whole number of pounds and also includes the original £1 paid to play the game, so in the example given the player actually wins £9.

Find the maximum prize fund that should be allocated to a single game in which fifteen turns are played. -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Data.Numbers.Primes as Primes

import qualified Data.MemoCombinators as Memo

import Data.Maybe
import Data.Ratio

import Control.Applicative

-- True means blue
-- 0-indexed
prob_of_red_on_turn :: Integer -> Bool -> Rational
prob_of_red_on_turn i outcome =
	if outcome
		then p
		else 1 - p
		where
			p :: Rational
			p = (%) 1 (fromInteger (i + 2))

rb_sequences' :: [Bool] -> [[Bool]]
rb_sequences' so_far =
	if length so_far == 15
		then [so_far]
		else (++)
			(rb_sequences' (True : so_far))
			(rb_sequences' (False : so_far))

rb_sequences :: [[Bool]]
rb_sequences = map reverse $ rb_sequences' []

wins :: [Bool] -> Bool
wins l = (ZList.count id l) > ((ZList.length' l) `div` 2)

sequence_probability :: [Bool] -> Rational
sequence_probability
	= product
	. map (\(i, outcome) -> prob_of_red_on_turn i outcome)
	. zip [0..]

rb_sequence_probabilities :: [(Bool, Rational)]
rb_sequence_probabilities
	= map (\l -> (wins l, sequence_probability l))
	$ rb_sequences

winning_probability :: Rational
winning_probability
	= sum
	. map snd
	. filter fst
	$ rb_sequence_probabilities

largest_payoff :: Integer
largest_payoff = floor $ (fromIntegral $ denominator winning_probability) / (fromIntegral $ numerator winning_probability)

main :: IO ()
main = do
	putStrLn . show $ largest_payoff
