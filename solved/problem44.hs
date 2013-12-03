
import Data.List
import Data.Ord
import qualified Data.Set as Set
import List

--is_int :: (Show a) => a -> Bool
is_int n = n == (fromInteger $ round n)

is_int2 :: (Show a) => a -> Bool
is_int2 n = ('0' == (last $ show n)) && ('.' == (last $ init $ show n))

pent :: Integer -> Integer
pent n = div (n * (3*n - 1)) 2

qf :: (Floating a) => (a -> a -> a) -> Integer -> a
qf op m = (1/6) * (op (fromInteger 1) (sqrt $ fromInteger (24 * m + 1)))

quad_formula :: (Floating a) => Integer -> [a]
quad_formula m = [qf (-) m, qf (+) m]

is_pentagonal :: Integer -> Bool
is_pentagonal n = or $ 
	map 
		(\m -> (is_int m) && (m >= 0)) 
		$ quad_formula n

diff_and_sum_pentagonal :: (Integer, Integer) -> Bool
diff_and_sum_pentagonal (a, b) = 
    is_pentagonal (b-a)
    && 
    is_pentagonal (b+a)

pentagonals :: [Integer] 
pentagonals = map pent [1..5000]--10000]

vvalid_differences = map
    (\x -> (x, diff_and_sum_pentagonal x))
    [(pentagonals !! i, b) | i <- [0..(length pentagonals)], b <- (drop (i+1) pentagonals)]

v = filter snd vvalid_differences

--valid_differences :: [Integer] 
valid_differences = filter 
    diff_and_sum_pentagonal
    [(pentagonals !! i, b) | i <- [0..(length pentagonals)], b <- (drop (i+1) pentagonals)]

--abs_diffs :: [Integer] 
abs_diffs = map (\(a, b) -> ((a, b), abs $ a - b)) valid_differences

sought = Data.List.minimumBy (comparing (\(t, diff) -> diff)) abs_diffs