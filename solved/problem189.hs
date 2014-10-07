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

other_colors :: Color -> [Color]
other_colors R = [B, G]
other_colors B = [R, G]
other_colors G = [R, B]

other_colors_2 :: Color -> Color -> [Color]
other_colors_2 R B = [G]
other_colors_2 B R = [G]
other_colors_2 B G = [R]
other_colors_2 G B = [R]
other_colors_2 G R = [B]
other_colors_2 R G = [B]
other_colors_2 R R = other_colors R
other_colors_2 B B = other_colors B
other_colors_2 G G = other_colors G

-- generalized <*>
-- [[a, b], [c, d]] -> [[a, c], [a, d], [b, c], [b, d]]
combine_lists :: [[a]] -> [[a]]
combine_lists [] = [[]]
combine_lists (list:lists) =
	(:) <$> list <*> combine_lists lists

map_sliding2_interleave :: (a -> a -> b) -> [a] -> [b]
map_sliding2_interleave _ [] = []
map_sliding2_interleave _ (x:[]) = []
map_sliding2_interleave f (x1:x2:xs) =
	(f x1 x2) : (map_sliding2_interleave f (x2:xs))

map_sliding2_interleave_exterior :: (a -> a -> b) -> (a -> b) -> [a] -> [b]
map_sliding2_interleave_exterior f g xs
	= [g $ head xs] ++ (map_sliding2_interleave f xs) ++ [g $ last xs]

next_colorings :: [(Row, Integer)] -> [(Row, Integer)]
next_colorings last_row_upwards
	= next_upwards
	where
		coalesce :: [(Row, Integer)] -> [(Row, Integer)]
		coalesce
			= map (\l -> (fst . head $ l, sum . map snd $ l))
			. List.groupBy (\a b -> fst a == fst b)
			. List.sortBy (Ord.comparing fst)

		next_downwards :: [(Row, Integer)]
		next_downwards
			= coalesce
			. normalize
			. concatMap (\(row, n) ->
				zip
					(combine_lists . map other_colors $ row)
					(repeat n)
				)
			$ last_row_upwards

		next_upwards :: [(Row, Integer)]
		next_upwards
			= coalesce
			. normalize
			. concatMap
				(\(row, n) ->
					zip 
						( combine_lists
						. map_sliding2_interleave_exterior
							other_colors_2
							other_colors 
						$ row )
						(repeat n)
				)
			. normalize
			$ next_downwards

normalize :: [(Row, Integer)] -> [(Row, Integer)]
normalize
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

num_colorings_at_row :: Int -> Integer
num_colorings_at_row row
	= sum
	. map snd
	. last
	. take row
	. iterate next_colorings
	$ [([R], 3)]

main :: IO ()
main = do
	putStrLn . show $ num_colorings_at_row 8
