{-
tarting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
-}

k = 20

safe_index arr m n
	| (m < 0) || (n < 0) = 0
	| otherwise = arr !! m !! n

n_paths m n arr
	| (m == 0) && (n == 0) = 1
	| otherwise = 
		safe_index arr (m-1) n
		+
		safe_index arr m (n-1)

arr = [[n_paths m n arr | n <- [0..k]] | m <- [0..k]]