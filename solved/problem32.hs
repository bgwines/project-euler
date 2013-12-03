
import Data.List
import Data.Char

is_pandigital :: String -> Bool
is_pandigital n = "123456789" == sort n

--get_two_factor_lists :: (Num a) => Integer -> [(Integer, a)]
get_two_factor_lists n = 
    map 
        (\m -> (m, div n m)) 
        $ filter 
            (\m -> n `mod` m == 0) 
            [2..(div n 2)]

get_two_factors_fast n = 
    case first_factor of Nothing -> (1, n)
                         Just factor -> (factor, div n factor)
    where first_factor = find (\m -> n `mod` m == 0) [2..(div n 2)]

is_pandigital_trio mult_pair = 
    is_pandigital $ 
        (show $ fst mult_pair) ++
        (show $ snd mult_pair) ++
        (show (fst mult_pair * snd mult_pair))

has_pandigital_trio l = Nothing /= find is_pandigital_trio l

sought = map (\(a,b) -> a*b) 
   $ map (\l -> head l) 
      $ filter has_pandigital_trio 
         $ map get_two_factor_lists [1..1000000]