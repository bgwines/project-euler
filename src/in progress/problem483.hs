{- We define a permutation as an operation that rearranges the order of the elements {1, 2, 3, ..., n}. There are n! such permutations, one of which leaves the elements in their initial order. For n = 3 we have 3! = 6 permutations:
- P1 = keep the initial order
- P2 = exchange the 1st and 2nd elements
- P3 = exchange the 1st and 3rd elements
- P4 = exchange the 2nd and 3rd elements
- P5 = rotate the elements to the right
- P6 = rotate the elements to the left

If we select one of these permutations, and we re-apply the same permutation repeatedly, we eventually restore the initial order.
For a permutation Pi, let f(Pi) be the number of steps required to restore the initial order by applying the permutation Pi repeatedly.
For n = 3, we obtain:
- f(P1) = 1 : (1,2,3) → (1,2,3)
- f(P2) = 2 : (1,2,3) → (2,1,3) → (1,2,3)
- f(P3) = 2 : (1,2,3) → (3,2,1) → (1,2,3)
- f(P4) = 2 : (1,2,3) → (1,3,2) → (1,2,3)
- f(P5) = 3 : (1,2,3) → (3,1,2) → (2,3,1) → (1,2,3)
- f(P6) = 3 : (1,2,3) → (2,3,1) → (3,1,2) → (1,2,3)

Let g(n) be the average value of f2(Pi) over all permutations Pi of length n.
g(3) = (12 + 22 + 22 + 22 + 32 + 32)/3! = 31/6 ≈ 5.166666667e0
g(5) = 2081/120 ≈ 1.734166667e1
g(20) = 12422728886023769167301/2432902008176640000 ≈ 5.106136147e3

Find g(350) and write the answer in scientific notation rounded to 10 significant digits, using a lowercase e to separate mantissa and exponent, as in the examples above. -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Data.Numbers.Primes as Primes

import qualified Data.MemoCombinators as Memo

import Data.Maybe
import Data.Ratio
import Data.Function

import Debug.Trace

import Data.Monoid
import Control.Monad
import Control.Applicative

type Permutation = [Int]

insert :: a -> [a] -> Int -> [a]
insert x xs i = (take i xs) ++ [x] ++ (drop i xs)

insert_at_all_indices :: a -> [a] -> [[a]]
insert_at_all_indices x xs
	= map (insert x xs) [0..(length xs)]

-- fact n-1 of these
cyclically_unique_permutations :: Int -> [Permutation]
cyclically_unique_permutations 1 = [[1]]
cyclically_unique_permutations 2 = [[1,2]]
cyclically_unique_permutations n
	= concatMap (tail . insert_at_all_indices n) (cyclically_unique_permutations $ n - 1)

cyclically_unique_permutations' :: Int -> [Permutation]
cyclically_unique_permutations' n
	= (++) (cyclically_unique_permutations n)
	. (\perm -> [perm])
	. take n
	. tail
	. cycle
	$ [1..n]

apply_permutation :: Permutation -> [a] -> [a]
apply_permutation perm list
	= map snd
	. List.sortBy (Ord.comparing fst)
	$ zip perm list

order :: Permutation -> Int
order perm
	= succ
	. length
	. takeWhile (not . ZList.is_sorted)
	. tail
	. iterate (apply_permutation perm)
	$ [1..(length perm)]

main :: IO ()
main = do
	putStrLn . show $ cyclically_unique_permutations 3
