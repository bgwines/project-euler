
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

qf :: (Floating a) => (a -> a -> a) -> Integer -> a
qf op m = (1/6) * (op (fromInteger 1) (sqrt $ fromInteger (24 * m + 1)))

quad_formula :: (Floating a) => Integer -> [a]
quad_formula m = [qf (-) m, qf (+) m]

is_pentagonal :: Integer -> Bool
is_pentagonal = any (\m -> (ZMath.is_int m) && (m >= 0)) . quad_formula

diff_and_sum_pentagonal :: (Integer, Integer) -> Bool
diff_and_sum_pentagonal (a, b) = and
	[ is_pentagonal (b - a)
	, is_pentagonal (b + a) ]

pentagonals :: [Integer] 
pentagonals = map pent [1..5000]--10000]
	where
		pent :: Integer -> Integer
		pent n = div (n * (3*n - 1)) 2

valid_differences :: [(Integer, Integer)]
valid_differences = filter 
	diff_and_sum_pentagonal
	[ (pentagonals !! i, b)
		| i <- [0..(length pentagonals)]
		, b <- (drop (i+1) pentagonals)]

abs_diffs :: [((Integer, Integer), Integer)]
abs_diffs = map (\(a, b) -> ((a, b), abs $ a - b)) valid_differences

main :: IO ()
main = do
	putStrLn . show $ List.minimumBy (Ord.comparing snd) abs_diffs

