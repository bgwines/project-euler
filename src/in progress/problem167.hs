
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.List as List

import Data.Maybe 

import Control.Applicative

type UlamCache = (Set.Set Int, Set.Set Int)

u :: (UlamCache, [Int]) -> (UlamCache, [Int])
u (cache, ulams) = (cache''', ulams ++ [new_n])
	where
		n :: Int
		n = last ulams

		new_n :: Int
		new_n = head . Set.toList . fst $ cache

		cache''' :: UlamCache
		cache''' = List.foldl' filter_evicted_cache cache'' (Set.toList $ snd cache'')
			where
				filter_evicted_cache :: UlamCache -> Int -> UlamCache
				filter_evicted_cache (x, curr_evicted) evicted_sum =
					if evicted_sum <= new_n
						then (x, Set.delete evicted_sum curr_evicted)
						else (x, curr_evicted)		

		cache'' :: UlamCache
		cache'' = List.foldl' filter_cache cache' (Set.toList $ fst cache')
			where
				filter_cache :: UlamCache -> Int -> UlamCache
				filter_cache (curr_sum_cache, curr_evicted) cached_sum =
					if cached_sum <= new_n
						then
							( Set.delete cached_sum curr_sum_cache
							, Set.insert cached_sum curr_evicted )
						else (curr_sum_cache, curr_evicted)

		cache' :: UlamCache
		cache' = List.foldl' update_cache cache new_sums
			where
				new_sums :: [Int]
				new_sums = map ((+) new_n) ulams

				update_cache :: UlamCache -> Int -> UlamCache
				update_cache (curr_sum_cache, curr_evicted) new_sum =
					if Set.member new_sum curr_sum_cache
						then
							( Set.delete new_sum curr_sum_cache
							, Set.insert new_sum curr_evicted )
						else if (new_sum > new_n) && (not $ Set.member new_sum curr_evicted)
							then
								( Set.insert new_sum curr_sum_cache
								, curr_evicted )
							else (curr_sum_cache, curr_evicted)

ulams :: Int -> Int -> Int -> [Int]
ulams a b ub
	= snd $
		(iterate u ((Set.fromList [a + b], Set.empty), [a, b])) !! ub

ulam_diffs :: Int -> Int -> Int -> [Int]
ulam_diffs a b ub = zipWith (-) (tail ulamsequence) ulamsequence
	where
		ulamsequence :: [Int]
		ulamsequence = ulams a b ub

find_repeat :: forall a. (Eq a) => [a] -> Maybe ([a], [a])
find_repeat l
	= verify_and_translate
	$ List.maximumBy
		(Ord.comparing match_length)
		[ (a, b)
			| a <- [0..(length l)]
			, b <- [(a+1)..(length l)] ]
	where
		verify_and_translate :: (Int, Int) -> Maybe ([a], [a])
		verify_and_translate (a, b)
			= if (repeating_section == second_repeat) && 
				 (repeating_section /= [])
				then Just (inital_section, repeating_section)
				else Nothing
				where
					inital_section :: [a]
					inital_section = take (a+1) l
					
					repeating_section :: [a]
					repeating_section = take (b - a) . drop (a + 1) $ l

					second_repeat :: [a]
					second_repeat = take (b - a) . drop (b + 1) $ l

		match_length :: (Int, Int) -> Int
		match_length (a, b) = match_length' (drop (a+1) l) (drop (b+1) l)
			where
				match_length' :: [a] -> [a] -> Int
				match_length' l1 l2
					= length
					. takeWhile id
					$ zipWith (==) l1 l2
