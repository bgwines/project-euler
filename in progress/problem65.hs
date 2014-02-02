
coeff_seq :: [Integer]
coeff_seq = [1..]--concat $ map (\n -> [1,2*n,1]) [1..10]

calc_term :: (Integer, Integer) -> Integer -> (Integer, Integer)
calc_term (a, b) k = ((k+1)*b + a, (k)*b + a)

terms :: [(Integer, Integer)]
terms = (2,1) : (scanl calc_term (3,2) coeff_seq)

valid :: (Integer, Integer) -> Bool
valid (a, b) = (length $ show a) > (length $ show b)

valid_terms = filter valid terms

sought = length valid_terms