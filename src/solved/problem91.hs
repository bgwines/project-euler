
import Euler
import Data.List
import Data.MemoCombinators as Memo

equals_epsilon :: Double -> Double -> Bool
equals_epsilon x y = abs (x - y) < 0.0001

dist :: (Integer, Integer) -> (Integer, Integer) -> Double
dist (x, y) (x', y') = sqrt $ fromIntegral $ (x - x')^2 + (y - y')^2

form_pythag_triple :: Double -> Double -> Double -> Bool
form_pythag_triple x y z =
	((x^2 + y^2) `equals_epsilon` (z^2)) ||
	((x^2 + z^2) `equals_epsilon` (y^2)) ||
	((z^2 + y^2) `equals_epsilon` (x^2))

forms_right_triangle :: ((Integer, Integer), (Integer, Integer), (Integer, Integer)) -> Bool
forms_right_triangle (a, b, c) =
	form_pythag_triple ab bc ac
	where
		ab = dist a b
		bc = dist b c
		ac = dist a c

calc_num_triangles :: Integer -> Integer -> Integer
calc_num_triangles 1 _ = 0
calc_num_triangles _ 1 = 0
calc_num_triangles n m = 
	let 
		n_interior = calc_num_triangles (n-1) m
		n_exterior =
			length' $ filter forms_right_triangle $ points
	in
		n_interior + n_exterior
	where
		all_points = [(n', m') | n' <- [1..n], m' <- [1..m]]
		new_points = [(n , m') | m' <- [1..m]]
		points = [(a, b, c) | b <- all_points, c <- new_points, a /= c, b /= c, a /= b, b <= c]
		a = (1, 1)

main = do
	(putStrLn . show) $ calc_num_triangles 51 51