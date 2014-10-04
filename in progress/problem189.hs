{- Consider the following configuration of 64 triangles:


We wish to colour the interior of each triangle with one of three colours: red, green or blue, so that no two neighbouring triangles have the same colour. Such a colouring shall be called valid. Here, two triangles are said to be neighbouring if they share an edge.
Note: if they only share a vertex, then they are not neighbours.

For example, here is a valid colouring of the above grid:


A colouring C' which is obtained from a colouring C by rotation or reflection is considered distinct from C unless the two are identical.

How many distinct valid colourings are there for the above configuration? -}

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

data Color = R | B | G deriving (Eq, Ord, Show)
type Row = [Color]
type SigMap = Map.Map Color Int

next_rows :: Row -> [Row]
next_rows [] = [[R], [B], [G]]
next_rows row
	= concatMap prepend_and_append
	. map (map_sliding2_interleave other_color)
	. filter repeat_colors
	. combine_lists
	. map other_colors
	$ row
	where
		other_colors :: Color -> [Color]
		other_colors R = [B, G]
		other_colors B = [R, G]
		other_colors G = [R, B]

		other_color :: Color -> Color -> Color
		other_color R B = G
		other_color B R = G
		other_color B G = R
		other_color G B = R
		other_color G R = B
		other_color R G = B
		other_color _ _ = error "same color"

		-- [[a, b], [c, d]] -> [[a, c], [a, d], [b, c], [b, d]]
		combine_lists :: [[a]] -> [[a]]
		combine_lists [] = [[]]
		combine_lists (list:lists) =
			(:) <$> list <*> combine_lists lists

		map_sliding2_interleave :: (a -> a -> a) -> [a] -> [a]
		map_sliding2_interleave _ [] = []
		map_sliding2_interleave _ (x:[]) = [x]
		map_sliding2_interleave f (x1:x2:xs) =
			x1 : (f x1 x2) : (map_sliding2_interleave f (x2:xs))

		repeat_colors :: Row -> Bool
		repeat_colors
			= all ((==) 1)
			. map length
			. List.group

		prepend_and_append :: Row -> [Row]
		prepend_and_append row
			= ZList.snoc
				<$> lasts
				<*> map (: row) heads
			where
				heads :: [Color]
				heads = other_colors (head row)

				lasts :: [Color]
				lasts = other_colors (last row)

colorings :: [(Row, Integer)] -> [(Row, Integer)]
colorings
	= symmetrify
	. map collect
	. List.groupBy (\a b -> fst a == fst b)
	. List.sortBy (Ord.comparing fst)
	. concatMap next_rows_with_counts
	where
		next_rows_with_counts :: (Row, Integer) -> [(Row, Integer)]
		next_rows_with_counts (row, n)
			= zip (next_rows row) (repeat n)

		collect :: [(Row, Integer)] -> (Row, Integer)
		collect rows_and_ns = (row, sum ns)
			where
				row :: Row
				row = head . map fst $ rows_and_ns

				ns :: [Integer]
				ns = map snd rows_and_ns

		-- improvement: length of last of output of `colorings`
		--     [1,1,2,8,128,32768] = 2^(2^k - 1)
		--   vs.
		--     above * 6
		symmetrify :: [(Row, Integer)] -> [(Row, Integer)]
		symmetrify
			= map coalesce_symmetric_rows
			. List.groupBy (\a b -> (cmp a) == (cmp b))
			. List.sortBy (Ord.comparing cmp)
			where
				cmp :: (Row, Integer) -> [Int]
				cmp = signature . fst

				coalesce_symmetric_rows :: [(Row, Integer)] -> (Row, Integer)
				coalesce_symmetric_rows rows =
					( fst . head $ rows
					, sum . map snd $ rows )

				signature :: Row -> [Int]
				signature row = map (mapping Map.!) row
					where
						mapping :: SigMap
						mapping = fst $ List.foldl' update (Map.empty, 1) row

						update :: (SigMap, Int) -> Color -> (SigMap, Int)
						update (m, i) c = if Map.member c m
							then (m, i)
							else (Map.insert c i m, i+1)

sought :: Int -> Integer
sought k
	= sum
	. map snd
	. last
	. take k
	. iterate colorings
	$ [([], 1)]

main :: IO ()
main = do
	putStrLn . show $ sought 8
