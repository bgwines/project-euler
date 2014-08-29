{- The most naive way of computing n15 requires fourteen multiplications:

n × n × ... × n = n15

But using a "binary" method you can compute it in six multiplications:

n × n = n2
n2 × n2 = n4
n4 × n4 = n8
n8 × n4 = n12
n12 × n2 = n14
n14 × n = n15

However it is yet possible to compute it in only five multiplications:

n × n = n2
n2 × n = n3
n3 × n3 = n6
n6 × n6 = n12
n12 × n3 = n15

We shall define m(k) to be the minimum number of multiplications to compute nk; for example m(15) = 5.

For 1 ≤ k ≤ 200, find ∑ m(k). -}

import qualified Data.MemoCombinators as Memo
import qualified Data.Ord as Ord
import qualified Data.List as List

import Control.Applicative

type MultComposition = [Integer]

minima_by :: (a -> a -> Ordering) -> [a] -> [a]
minima_by cmp [] = []
minima_by cmp (x:xs) = minima_by' cmp xs [x]
	where
		minima_by' :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
		minima_by' _ [] so_far = so_far
		minima_by' cmp (x:xs) so_far =
			case x `cmp` (head so_far) of
				LT -> minima_by' cmp xs [x]
				EQ -> minima_by' cmp xs (x : so_far)
				GT -> minima_by' cmp xs so_far

optimal_compositions :: Integer -> [MultComposition]
optimal_compositions = Memo.integral optimal_compositions'

optimal_compositions' :: Integer -> [MultComposition]
optimal_compositions' 0 = [[]]
optimal_compositions' 1 = [[]]
optimal_compositions' 2 = [[2]]
optimal_compositions' n
	= minima_by (Ord.comparing length) compositions
	where
		mult_pairs :: [(Integer, Integer)]
		mult_pairs = map (\k -> (k, n-k)) [1..(n `div` 2)]

		compositions :: [MultComposition]
		compositions = concatMap
			(combine . pair_map optimal_compositions)
			mult_pairs

		combine :: ([MultComposition], [MultComposition]) -> [MultComposition]
		combine (mcs, mcs')
			= map (List.nub . (:) n)
			$ (++) <$> mcs <*> mcs'

		pair_map :: (a -> b) -> (a, a) -> (b, b)
		pair_map f (a, a') = (f a, f a')

min_num_mults :: Integer -> Integer
min_num_mults
	= toInteger
	. length
	. head
	. optimal_compositions

main :: IO ()
main = do
	putStrLn . show $ sum . map min_num_mults $ [1..200]
