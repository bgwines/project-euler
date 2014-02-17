{-
Consider quadratic Diophantine equations of the form:

x2 – Dy2 = 1

For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.

It can be assumed that there are no solutions in positive integers when D is square.

By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:

3^2 – 2×2^2 = 1
2^2 – 3×1^2 = 1
9^2 – 5×4^2 = 1
5^2 – 6×2^2 = 1
8^2 – 7×3^2 = 1

Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.

Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.
-}

import Euler
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.MemoCombinators as Memo

solves :: Integer -> (Integer, Integer) -> Bool
solves d (x, y) = (x^2 - (d * y^2)) == 1

calc_minimal_sol_in_x :: Integer -> Integer
calc_minimal_sol_in_x d = fst $ find_guaranteed (solves d) (sqrt_convergents d)

ds_and_xs :: [(Integer, Integer)]
ds_and_xs = map_keep calc_minimal_sol_in_x ds
	where
		ds = filter (not . is_square) [1..1000]
		is_square = is_int . sqrt . fromIntegral

main = do
	putStrLn . show . fst . List.maximumBy (Ord.comparing snd) $ ds_and_xs
