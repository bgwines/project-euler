
gen :: (Double -> Double) -> [Double]
gen f = [f n | n <- [1..]]

pentagonals :: [Double]
pentagonals = gen pent
	where
		pent :: Double -> Double
		pent n = n * (3*n - 1) / 2

hexagonals :: [Double]
hexagonals = gen hex
	where
		hex :: Double -> Double
		hex n = n * (2*n - 1)

common_elems :: [Integer] -> [Integer] -> [Integer]
common_elems [] ys = ys
common_elems xs [] = xs
common_elems (x:xs) (y:ys)
	| x == y = x : common_elems xs ys
	| x <  y = common_elems xs (y:ys)
	| x >  y = common_elems (x:xs) ys

main :: IO ()
main = do
	putStrLn . show $ common_elems pentagonals hexagonals