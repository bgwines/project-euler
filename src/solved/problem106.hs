{- Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:

S(B) ≠ S(C); that is, sums of subsets cannot be equal.
If B contains more elements than C then S(B) > S(C).
For this problem we shall assume that a given set contains n strictly increasing elements and it already satisfies the second rule.

Surprisingly, out of the 25 possible subset pairs that can be obtained from a set for which n = 4, only 1 of these pairs need to be tested for equality (first rule). Similarly, when n = 7, only 70 out of the 966 subset pairs need to be tested.

For n = 12, how many of the 261625 subset pairs that can be obtained need to be tested for equality?

NOTE: This problem is related to Problem 103 and Problem 105. -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.List as List

import qualified Data.MemoCombinators as Memo

import Data.Maybe

import Control.Applicative

ub :: Int
ub = 12

less :: (Ord a) => [a] -> [a] -> Bool
less l1 l2 = and $ zipWith (<) l1 l2

share_elems :: (Ord a) => [a] -> [a] -> Bool
share_elems a b = (length . ZList.uniqueify $ a ++ b) /= (length a + length b)

subsets_of_equal_lengths :: [[[Int]]]
subsets_of_equal_lengths = List.groupBy (\a b -> length a == length b) . List.sortBy (Ord.comparing length) . ZList.powerset $ [1..ub]

pairs_of_equal_lengths :: [[([Int], [Int])]]
pairs_of_equal_lengths
	= map (ZList.uniqueify . (pair ZList.<$*>))
	$ subsets_of_equal_lengths
	where
		pair :: [Int] -> [Int] -> ([Int], [Int])
		pair a b = if a < b
			then (a, b)
			else (b, a)

disjoint_pairs_of_equal_lengths :: [[([Int], [Int])]]
disjoint_pairs_of_equal_lengths
	= map
		( map (ZList.map_pair List.sort List.sort)
		. filter (\(a, b) -> not $ share_elems a b) )
	$ pairs_of_equal_lengths

need_testing :: [([Int], [Int])]
need_testing = concatMap (filter (\(a, b) -> not $ less a b)) disjoint_pairs_of_equal_lengths

sought :: Int
sought = length need_testing

main :: IO ()
main = do
	putStrLn . show $ sought
