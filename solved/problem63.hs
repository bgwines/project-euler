{-
The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit number, 134217728=89, is a ninth power.

How many n-digit positive integers exist which are also an nth power?
-}

count_num_powers_of_length_power n = 
	let lengths_and_powers = [((length $ show (n^x)), x) | x <- [1..50]] in
	length 
		$ takeWhile (==EQ) 
			$ map (\(a,b) -> compare a b) lengths_and_powers

sought = sum [count_num_powers_of_length_power i | i <- [1..30]]

main = do
	(putStrLn . show) sought