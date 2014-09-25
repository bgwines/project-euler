{-# LANGUAGE ScopedTypeVariables #-}

{- Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.

Peter and Colin roll their dice and compare totals: the highest total wins. The result is a draw if the totals are equal.

What is the probability that Pyramidal Pete beats Cubic Colin? Give your answer rounded to seven decimal places in the form 0.abcdefg -}

import qualified Data.List as List
import qualified Zora.List as ZList

import Control.Applicative

subsets_of_size_with_replacement :: Integer -> [a] -> [[a]]
subsets_of_size_with_replacement n src = subsets_of_size_with_replacement' n src []

subsets_of_size_with_replacement' :: forall a. Integer -> [a] -> [a] -> [[a]]
subsets_of_size_with_replacement' 0 src so_far = [so_far]
subsets_of_size_with_replacement' n src so_far
	= concatMap (subsets_of_size_with_replacement' (n-1) src) $ nexts
	where
		nexts :: [[a]]
		nexts = map (: so_far) src

pete_outcomes :: [Integer]
pete_outcomes
	= map sum $ subsets_of_size_with_replacement 9 [1..4]

colin_outcomes :: [Integer]
colin_outcomes
	= map sum $ subsets_of_size_with_replacement 6 [1..6]

elem_counts :: (Ord a) => [a] -> [(a, Integer)]
elem_counts
	= map (\l -> (head l, ZList.length' l))
	. List.group
	. List.sort

pete_distribution :: [(Integer, Integer)]
pete_distribution = elem_counts pete_outcomes

colin_distribution :: [(Integer, Integer)]
colin_distribution = elem_counts colin_outcomes

pairs :: [(Bool, Integer)]
pairs = f <$> pete_distribution <*> colin_distribution
	where
		f :: (Integer, Integer) -> (Integer, Integer) -> (Bool, Integer)
		f (p, pn) (c, cn) = (p > c, pn * cn)

total :: Integer
total = sum . map snd $ pairs

p_wins :: Integer
p_wins = sum . map snd . filter fst $ pairs

main :: IO ()
main = do
	putStrLn . take (1 + (length "0.")) . show $ (fromIntegral p_wins) / (fromIntegral total)
