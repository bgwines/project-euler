k = 500

b True = 1
b False = 0

calc_num_factors :: Integer -> Integer
calc_num_factors n = sum $ map b [(n `mod` k) == 0 | k <- [1..n]]

triangles = scanl1 (+) [1..]

factors_of_triangles = map (\t -> (t, calc_num_factors t)) triangles

sought_pair = find (\(a,b) -> b > k) factors_of_triangles