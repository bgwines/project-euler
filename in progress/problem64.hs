{-
The first ten continued fraction representations of (irrational) square roots are:

√2=[1;(2)], period=1
√3=[1;(1,2)], period=2
√5=[2;(4)], period=1
√6=[2;(2,4)], period=2
√7=[2;(1,1,1,4)], period=4
√8=[2;(1,4)], period=2
√10=[3;(6)], period=1
√11=[3;(3,6)], period=2
√12= [3;(2,6)], period=2
√13=[3;(1,1,1,1,6)], period=5

Exactly four continued fractions, for N ≤ 13, have an odd period.

How many continued fractions for N ≤ 10000 have an odd period?
-}

import Euler
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.MemoCombinators as Memo

odd_period_continued_fraction_sqrts :: [Integer]
odd_period_continued_fraction_sqrts = 
	filter (is_odd_period . continued_fraction_sqrt) irr_squares
		where
			is_odd_period = odd . length . tail

main = do
	(putStrLn . show) $ length odd_period_continued_fraction_sqrts
