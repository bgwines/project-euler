
import Data.List
import Data.Ord
import Prime

ub = 100

solves (x,y,d)= x^2 - (d * y^2) == 1

get_min_sol d = 
	find solves [(x,y,d) | x <- [1..ub^2], y <- [1..ub^2]]

cmp a = case a of Just(x,y,d) -> x
                  Nothing -> 0

not_square n = (sqrt n) /= (fromIntegral $ ceiling $ sqrt n)

ds = filter not_square [1..ub]
sought = maximumBy (comparing cmp) [get_min_sol d | d <- ds]