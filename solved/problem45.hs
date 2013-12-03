

pent :: Double -> Double
pent n = n * (3*n - 1) / 2

hex :: Double -> Double
hex n = n * (2*n - 1)

gen f = [f n | n <- [1..]]

pentagonals = gen pent
hexagonals = gen hex

find_common_elems [] ys = ys
find_common_elems xs [] = xs
find_common_elems (x:xs) (y:ys)
	| x == y = x : find_common_elems xs ys
	| x <  y = find_common_elems xs (y:ys)
	| x >= y = find_common_elems (x:xs) ys