
terms :: [(Integer, Integer)]
terms = scanl calc_term (3,2) [1..1000]
	where
		calc_term :: (Integer, Integer) -> Integer -> (Integer, Integer)
		calc_term (a, b) _ = (b + a + b, a + b)

num_valid_terms :: Int
num_valid_terms = length . filter valid $ terms
	where
		valid :: (Integer, Integer) -> Bool
		valid (a, b) = (length . show $ a) > (length . show $ b)

main :: IO ()
main = do
	putStrLn . show $ num_valid_terms