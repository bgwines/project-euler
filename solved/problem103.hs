
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
