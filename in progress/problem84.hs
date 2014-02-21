{-
-}

import Euler
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.MemoCombinators as Memo

type Card = String
type Square = String
type Roll = (Integer, Integer)

sentinel_roll :: Roll
sentinel_roll = (-1, -1)

cc_cards_src :: [Card]
cc_cards_src = cycle . shuffle . concat $ [["_"]]--[["GO", "JAIL"], replicate (16-2) "_"]

ch_cards_src :: [Card]
ch_cards_src = cycle . shuffle . concat $ [["_"]]--[["GO", "JAIL", "C1", "E3", "H2", "R1", "NEXTR", "NEXTR", "NEXTU", "-3"], replicate (16-10) "_"]

squares_finite_src :: [Square]
squares_finite_src = ["GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2", "B3", "JAIL", "C1", "U1", "C2", "C3", "R2", "D1", "CC2", "D2", "D3", "FP", "E1", "CH2", "E2", "E3", "R3", "F1", "F2", "U2", "F3", "G2J", "G1", "G2", "CC3", "G3", "R4", "CH3", "H1", "T2", "H2"]

squares_src :: [Square]
squares_src = cycle squares_finite_src

dice_rolls_src :: [Roll]
dice_rolls_src = preprocess . pairify $ random_integers (1, 6) 319005965
	where
		preprocess :: [(Integer, Integer)] -> [(Integer, Integer)]
		preprocess = concat . map adjust_group . List.group

		adjust_group :: [(Integer, Integer)] -> [(Integer, Integer)]
		adjust_group l = if length l < 3
			then l
			else (replicate 2 (h l)) ++ (replicate ((length l) - 2) sentinel_roll)

calc_num_squares_to_advance :: [Square] -> Card -> Roll -> Integer
calc_num_squares_to_advance squares card roll@(r1, r2)= case card of
	"_" -> r1 + r2
	"-3" -> -3 + r1 + r2 --won't be 100% accurate in the case where r1 == r2 == 1.
	"NEXTR" -> minimum . map (n_until squares) $ ["R1", "R2", "R3", "R4"]
	"NEXTU" -> minimum . map (n_until squares) $ ["U1", "U2"]
	_ -> n_until squares card

n_until :: [Square] -> Card -> Integer
n_until squares card = length' $ takeWhile (/= card) squares

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
		then h squares
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
	=
	h squares' : get_squares_hit' dice_rolls' cc_cards' ch_cards' squares'
	where
		dice_rolls' = t dice_rolls
		squares' = drop num_to_drop' squares
		cc_cards' = if hit_square_pre_cards squares roll == "CC*" then t cc_cards else cc_cards
		ch_cards' = if hit_square_pre_cards squares roll == "CH*" then t ch_cards else ch_cards

		num_to_drop' :: Int
		num_to_drop' = num_to_drop cc ch squares roll

len = 1000000.0

squares_hit :: [Square]
squares_hit = take (round len)
	$ get_squares_hit'
		dice_rolls_src
		cc_cards_src
		ch_cards_src
		squares_src

frequencies :: [(Square, Double)]
frequencies =
	reverse . List.sortBy (Ord.comparing snd)
		$ map_keep calc_frequency squares_finite_src
	where
		calc_frequency =
			(/ len) . fromIntegral . length' . (flip List.elemIndices $ squares_hit)

modal_string :: String
modal_string = 
	concat
		. map
			(show
				. from_just 
					. (flip List.elemIndex $ squares_finite_src)
						. fst
			)
	$ take 3 frequencies

main = do
	(putStrLn . show) $ modal_string

