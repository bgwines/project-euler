
hypotenuses :: [Integer]
hypotenuses = 

-- insufficient: see ∆OPQ from problem statement
-- TODO: Data.memocombinators
calc_num_triangles :: Integer -> Integer -> Integer
calc_num_triangles 1 1 = 4
calc_num_triangles n m = 
	2 * calc_num_triangles (n) (m-1) + 
	2 * calc_num_triangles (n-1) (m)
	-- + #hypotenuses


sought = calc_num_triangles 2 2--49 49