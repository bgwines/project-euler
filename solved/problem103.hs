{- Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:

S(B) ≠ S(C); that is, sums of subsets cannot be equal.
If B contains more elements than C then S(B) > S(C).
If S(A) is minimised for a given n, we shall call it an optimum special sum set. The first five optimum special sum sets are given below.

n = 1: {1}
n = 2: {1, 2}
n = 3: {2, 3, 4}
n = 4: {3, 5, 6, 7}
n = 5: {6, 9, 11, 12, 13}

It seems that for a given optimum set, A = {a1, a2, ... , an}, the next optimum set is of the form B = {b, a1+b, a2+b, ... ,an+b}, where b is the "middle" element on the previous row.

By applying this "rule" we would expect the optimum set for n = 6 to be A = {11, 17, 20, 22, 23, 24}, with S(A) = 117. However, this is not the optimum set, as we have merely applied an algorithm to provide a near optimum set. The optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25}, with S(A) = 115 and corresponding set string: 111819202225.

Given that A is an optimum special sum set for n = 7, find its set string. -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.List as List

import Data.Maybe

import Control.Applicative

type SpecialSumSet = ([Int], [Int], [Int])

optimal_special_sum_sets_of_length :: Int -> [[Int]]
optimal_special_sum_sets_of_length len
	= ZList.running_bests_by (flip (Ord.comparing sum))
	. concat
	. filter ((/=) [])
	. map (optimal_special_sum_sets_of_length_with_start len)
	$ [[a, b] | a <- [1..35], b <- [(a+1)..35]]

optimal_special_sum_sets_of_length_with_start :: Int -> [Int] -> [[Int]]
optimal_special_sum_sets_of_length_with_start len start
	= ZList.minima_by (Ord.comparing sum)
	$ special_sum_sets_of_length' len start

special_sum_sets_of_length' :: Int -> [Int] -> [[Int]]
special_sum_sets_of_length' len so_far
	| ((succ $ length so_far) == len) = next_sets
	| otherwise = concatMap (special_sum_sets_of_length' len) next_sets
	where
		next_sets :: [[Int]]
		next_sets = map (\n -> so_far ++ [n]) next_elems

		next_elems :: [Int]
		next_elems
			= filter satisfies_subset_equality_property
			. takeWhile satisfies_monotonic_property
			$ [(succ $ last so_far)..]

		satisfies_monotonic_property :: Int -> Bool
		satisfies_monotonic_property n
			= and
			$ zipWith
				(flip (<))
				(prefix_sums)
				(suffix_sums)
			where
				prefix_sums :: [Int]
				prefix_sums = tail $ scanl1 (+) (so_far ++ [n])

				suffix_sums :: [Int]
				suffix_sums = scanl1 (+) . reverse $ (so_far ++ [n])

		satisfies_subset_equality_property :: Int -> Bool
		satisfies_subset_equality_property n
			= all (all (((==) 1) . snd))
			. map (ZList.elem_counts . map sum)
			. List.groupBy (\a b -> length a == length b)
			. List.sortBy (Ord.comparing length)
			. ZList.powerset
			$ n : so_far

main :: IO ()
main = do
	putStrLn . show $ foldl (++) . map show $ optimal_special_sum_sets_of_length 7

-- optimizations:

	-- checking S /= preperty:
		-- only need to check subsets of equal size (by Property 2)
		-- only need to check subset pairs of sizes >=2
		-- 
		-- for now, just check manually
		--     minor optimization: don't take powerset every time (only 2nd half will contain n)

	-- check if monotonic(S, (<)) property is maintained:
			-- only need to compare top 1 against bottom 2, top 2 against bottom 3, etc.
				-- can cache bottom levels

	-- for each x:
		-- can cache top 1 / 2 / 3 from previous iterations
			-- maybe even from previous recursion - n=7 might be too small for this to matter - yeah; just prepend and map ((+) k)
		-- if doesn't satisfy, can abort inner loop early
							-- can abort outer loop early
							-- can abort recursion -- cannot satisfy current list.
