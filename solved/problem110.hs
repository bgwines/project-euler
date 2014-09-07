{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.List as List

import Data.Maybe

import Control.Applicative

ub :: Integer
ub = 4000000

num_solutions_from_exponents :: [Integer] -> Integer
num_solutions_from_exponents 
	= succ
	. (\m -> m `div` 2)
	. product
	. map (succ . (*) 2) -- succ because p_i^0 is a valid choice

-- 15 lists of respective lengths 1..15
-- list 15 is [1,1,1,1,1...,1,1]
-- list 14 might have a 2 or even a 3
best_exponent_list_of_length :: Integer -> [Integer]
best_exponent_list_of_length len
	= List.minimumBy (Ord.comparing num_solutions_from_exponents)
	$ valid_exponent_lists' [replicate (fromInteger len) 1]

valid_exponent_lists' :: [[Integer]] -> [[Integer]]
valid_exponent_lists' [] = []
valid_exponent_lists' exp_lists
	= greater ++ (valid_exponent_lists' less)
	where
		greater :: [[Integer]]
		greater = filter ((> ub) . num_solutions_from_exponents) exp_lists_incremented

		less :: [[Integer]]
		less = filter ((< ub) . num_solutions_from_exponents)  exp_lists_incremented

		-- for each one in exp_lists, try all ways to increment an exponent
		-- e.g. [2,1,1] -> [3,1,1] or [2,2,1] (but not (2,1,2))
		exp_lists_incremented :: [[Integer]]
		exp_lists_incremented
			= ZList.uniqueify
			. concatMap (increment_in_all_ways . divide_exp_list)
			$ exp_lists
			where
				increment_in_all_ways :: ([Integer], [Integer]) -> [[Integer]]
				increment_in_all_ways (a, b)
					= ZList.uniqueify
					$ [ (concat $ increment_first_of_ith i a') ++ b
						| i <- [0..((length a') - 1)] ]
						where
							a' :: [[Integer]]
							a' = List.group a

				increment_first_of_ith :: Int -> [[Integer]] -> [[Integer]]
				increment_first_of_ith i l =
					if i == length l - 1
						then (init l) ++ [increment_first $ last l]
						else if i == 0
							then [increment_first $ head l] ++ (tail l)
							else (take i l) ++ [increment_first $ l !! i] ++ (tail $ drop i l)
						where
							increment_first :: [Integer] -> [Integer]
							increment_first (x:xs) = (succ x) : xs

				-- [3,2,1,1,1] -> ([3,2,1], [1,1])
				divide_exp_list :: [Integer] -> ([Integer], [Integer])
				divide_exp_list
					= (\(a, b) -> if b == []
						then (a, b)
						else (a ++ [1], tail b))
					. ZList.take_while_and_rest (> 1)

turn_into_number :: [Integer] -> Integer
turn_into_number
	= product
	. map (\(p, a) -> p^a)
	. zip ZMath.primes

sought :: Integer
sought
	= minimum
	. map turn_into_number
	. map best_exponent_list_of_length
	$ [15,14..8]

main :: IO ()
main = do
	putStrLn . show $ sought
