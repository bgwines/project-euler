
import List
import qualified Data.List

all_digits :: [Integer]
all_digits = [1..10]

calc_valid_tuples_rec :: [(Integer, Integer, Integer)] -> [[(Integer, Integer, Integer)]] 
calc_valid_tuples_rec so_far = 
    -- TODO: check that fst of b.c. is not negative && doesn't re-use number
    case depth of 0 -> [so_far ++ [(tuple_sum - prev_t_trd - init_t_snd, prev_t_trd, init_t_snd)]]
                  _ -> concat $ map 
                      get_valid_sub_level_tuples 
                      curr_level_valid_tuples
        where curr_level_valid_tuples = [(a, prev_t_trd, c) | a <- usable_digits, c <- usable_digits, (a + prev_t_trd + c) == tuple_sum, a /= c]
              get_valid_sub_level_tuples t = calc_valid_tuples_rec $ so_far ++ [t]
              prev_t_trd = trd3 $ last so_far
              init_t_snd = snd3 $ head so_far
              tuple_sum = (\(a, b, c) -> a + b + c) $ head so_far
              depth = (5-1) - (length so_far)
              usable_digits = [e | e <- all_digits, not (e `elem` so_far_as_list)]
              so_far_as_list = concat $ map (\(a, b, c) -> [a, b, c]) so_far

calc_valid_tuples :: [(Integer, Integer, Integer)] -> [[(Integer, Integer, Integer)]] 
calc_valid_tuples inital_tuples = 
    filter last_one_valid $ 
        concat [calc_valid_tuples_rec [t] | t <- inital_tuples]
        where last_one_valid l = 
                  ((questionable_digit l) >= 1) && 
                  ((questionable_digit l) `elem` all_digits) && 
                  (not ((questionable_digit l) `elem` (rest_of_l l)))
              questionable_digit l = (fst3 $ last l)
              rest_of_l l = (concat $ map (\(a, b, c) -> [a, b, c]) (init l)) ++ [(snd3 $ last l)] ++ [(snd3 $ last l)]

calc_all_valid_tuples :: [[(Integer, Integer, Integer)]]
calc_all_valid_tuples = calc_valid_tuples inital_tuples
    where inital_tuples = [(a, b, c) | a <- all_digits, b <- all_digits, c <- all_digits, a /= b, b /= c, c /= a]

custom_sort_rec :: [(Integer, Integer, Integer)] -> (Integer, Integer, Integer) -> [(Integer, Integer, Integer)]
custom_sort_rec l min_elem
    | ((head l) == min_elem) = l
    | otherwise = custom_sort_rec ((tail l) ++ [head l]) min_elem

custom_sort :: [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
custom_sort l = custom_sort_rec l $ minimum l

sorted_concatenated_valid_tuples :: [String]
sorted_concatenated_valid_tuples = 
    filter
        (\str -> (length str) == 16)
        $ map (foldl1 (++)) sorted_as_strs
    where sorted_as_strs = map
              (map (\(a, b, c) -> show a ++ show b ++ show c))
              sorted 
    	  sorted = map custom_sort calc_all_valid_tuples
          minFirstElem t1 t2 = compare 
            ((head . head) t1) 
            ((head . head) t2)

main = do
	putStrLn $ maximum sorted_concatenated_valid_tuples
