{- In a 3x2 cross-hatched grid, a total of 37 different rectangles could be situated within that grid as indicated in the sketch.


There are 5 grids smaller than 3x2, vertical and horizontal dimensions being important, i.e. 1x1, 2x1, 3x1, 1x2 and 2x2. If each of them is cross-hatched, the following number of different rectangles could be situated within those smaller grids:

1x1: 1 
2x1: 4 
3x1: 8 
1x2: 4 
2x2: 18

Adding those to the 37 of the 3x2 grid, a total of 72 different rectangles could be situated within 3x2 and smaller grids.

How many different rectangles could be situated within 47x43 and smaller grids? -}

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

n_nonempty_contiguous_subsequences :: Int -> Int
n_nonempty_contiguous_subsequences n = n * (n + 1) `div` 2

f :: Int -> Int -> Int
f = Memo.memo2 Memo.integral Memo.integral f'

f' :: Int -> Int -> Int
f' 0 _ = 0
f' _ 0 = 0
f' 1 m
	= (n_nonempty_contiguous_subsequences m)
	+ (m - 1)
f' n 1
	= (n_nonempty_contiguous_subsequences n)
	+ (n - 1)
f' n m -- n, m >= 2
	= inclusion
	- exclusion
	+ new_verticals
	+ new_diagonals
	+ debug
	where
		debug :: Int
		debug
			= traceShow ("n: " ++ show n ++ ", m: " ++ show m)
			. traceShow ("    inclusion: " ++ show inclusion)
			. traceShow ("    exclusion: " ++ show exclusion)
			. traceShow ("    new_verticals: " ++ show new_verticals)
			. traceShow ("    n_new_diags_of_full_length: " ++ show n_new_diags_of_full_length)
			. traceShow ("    n_new_diags_of_almost_full_length: " ++ show n_new_diags_of_almost_full_length)
			. traceShow ("    total: ", inclusion - exclusion + new_verticals + new_diagonals)
			. traceShow ("")
			$ 0

		inclusion :: Int
		inclusion = 2 * (f n (m - 1))

		exclusion :: Int
		exclusion = f n (m - 2)

		new_verticals :: Int
		new_verticals = n_nonempty_contiguous_subsequences n

		new_diagonals :: Int
		new_diagonals = n_new_diags_of_full_length + n_new_diags_of_almost_full_length 

		n_new_diags_of_full_length :: Int
		n_new_diags_of_full_length
			= (*) 2 -- diags going up in 2 directions: L and R
			. max 0 -- no new full diags if m >= n
			$ n - m

		n_new_diags_of_almost_full_length :: Int
		n_new_diags_of_almost_full_length
			= if m > n
				then 0
				else 2 * 2

main :: IO ()
main = do
	putStrLn . show $ 142857
