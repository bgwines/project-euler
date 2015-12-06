{-
-}

import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.MemoCombinators as Memo

import qualified Zora.List as ZList

import Data.Maybe

import System.Random


type Card = String
type Square = String
type Roll = (Integer, Integer)

sentinel_roll :: Roll
sentinel_roll = (-1, -1)

shuffle' :: (Eq a) => [a] -> [a]
shuffle' = flip ZList.shuffle $ 280172349

cc_cards_src :: [Card]
cc_cards_src
	= cycle
	. shuffle'
	. concat
	$ [["GO", "JAIL"], replicate (16-2) "_"]

ch_cards_src :: [Card]
ch_cards_src
	= cycle
	. shuffle'
	. concat
	$ [["GO", "JAIL", "C1", "E3", "H2", "R1", "NEXTR", "NEXTR", "NEXTU", "-3"], replicate (16-10) "_"]

squares_finite_src :: [Square]
squares_finite_src = ["GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2", "B3", "JAIL", "C1", "U1", "C2", "C3", "R2", "D1", "CC2", "D2", "D3", "FP", "E1", "CH2", "E2", "E3", "R3", "F1", "F2", "U2", "F3", "G2J", "G1", "G2", "CC3", "G3", "R4", "CH3", "H1", "T2", "H2"]

squares_src :: [Square]
squares_src = cycle squares_finite_src

dice_rolls_src :: [Roll]
dice_rolls_src
	= preprocess
	. ZList.pairify
	$ random_integers (1, 4) 193105965
	where
		preprocess :: [(Integer, Integer)] -> [(Integer, Integer)]
		preprocess = concat . map adjust_group . List.group

		adjust_group :: [(Integer, Integer)] -> [(Integer, Integer)]
		adjust_group l = if length l < 3
			then l
			else (replicate 2 (head l)) ++ (replicate ((length l) - 2) sentinel_roll)

calc_num_squares_to_advance :: [Square] -> Card -> Roll -> Integer
calc_num_squares_to_advance squares card roll@(r1, r2)= case card of
	"_" -> r1 + r2
	"NEXTR" -> minimum . map (n_until squares) $ ["R1", "R2", "R3", "R4"]
	"NEXTU" -> minimum . map (n_until squares) $ ["U1", "U2"]
	"-3" -> let x = -3 + r1 + r2 in
		if x >= 0 then x else
			(ZList.length' squares_finite_src) - 1
	_ -> n_until squares card

n_until :: [Square] -> Card -> Integer
n_until squares card
	= ZList.length'
	. takeWhile (/= card)
	$ squares

num_to_drop :: Card -> Card -> [Square] -> Roll -> Int
num_to_drop cc ch squares roll@(r1, r2) = fromInteger $
	if roll == sentinel_roll
		then n_until squares "JAIL"
		else case hit_square_pre_cards squares roll of
			"G2J" -> n_until squares "JAIL"
			"CC*" -> calc_num_squares_to_advance squares cc roll
			"CH*" -> calc_num_squares_to_advance squares ch roll
			_ -> r1 + r2

hit_square_pre_cards :: [Square] -> Roll -> Square
hit_square_pre_cards squares roll@(r1, r2) =
	if roll == sentinel_roll
		then head squares
		else case x' of
			"CC" -> "CC*"
			"CH" -> "CH*"
			_ -> x
			where
				x' = take 2 x
				x = squares !! (fromInteger $ r1 + r2)

get_squares_hit' :: [Roll] -> [Card] -> [Card] -> [Square] -> [Square]
get_squares_hit'
	dice_rolls@(roll:dice_rolls')
	cc_cards@(cc:_)
	ch_cards@(ch:_)
	squares
	= (:) (head squares') $ get_squares_hit' dice_rolls' cc_cards' ch_cards' squares'
	where
		dice_rolls' :: [Roll]
		dice_rolls' = tail dice_rolls

		squares' :: [Square]
		squares' = drop num_to_drop' squares

		cc_cards' :: [Card]
		cc_cards' = if hit_square_pre_cards squares roll == "CC*" then tail cc_cards else cc_cards

		ch_cards' :: [Card]
		ch_cards' = if hit_square_pre_cards squares roll == "CH*" then tail ch_cards else ch_cards

		num_to_drop' :: Int
		num_to_drop' = num_to_drop cc ch squares roll

len :: Double
len = 1000000.0

squares_hit :: [Square]
squares_hit = take (round len)
	$ get_squares_hit'
		dice_rolls_src
		cc_cards_src
		ch_cards_src
		squares_src

random_integers :: (Integer, Integer) -> Integer -> [Integer]
random_integers range seed = randomRs range . mkStdGen $ fromInteger seed

frequencies :: [(Square, Double)]
frequencies
	= reverse
	. List.sortBy (Ord.comparing snd)
	. ZList.map_keep calc_frequency
	$ squares_finite_src
	where
		calc_frequency :: Square -> Double
		calc_frequency
			= (/ (len))
			. fromIntegral
			. length
			. (flip List.elemIndices $ squares_hit)

modal_string :: String
modal_string = 
	concatMap
		( show
		. fromJust 
		. (flip List.elemIndex $ squares_finite_src)
		. fst )
	$ take 3 frequencies

main :: IO ()
main = do
	(putStrLn . show) $ modal_string

