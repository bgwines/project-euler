
calc_term :: (Integer, Integer) -> Integer -> (Integer, Integer)
calc_term (a, b) _ = (b+a+b, a+b)

terms :: [(Integer, Integer)]
terms = scanl calc_term (3,2) [1..1000]

valid :: (Integer, Integer) -> Bool
valid (a, b) = (length $ show a) > (length $ show b)

valid_terms = filter valid terms

sought = length valid_terms