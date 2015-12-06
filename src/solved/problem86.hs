
import qualified Data.List

is_int x = x == fromInteger (round x)

calc_min_path :: Integer -> Integer -> Integer -> Double
calc_min_path w l h = (sqrt . fromInteger . minimum) [
    l^2 + (w+h)^2,
    w^2 + (l+h)^2,
    h^2 + (l+w)^2]

calc_n_int_min_paths_up_to :: Integer -> Integer
calc_n_int_min_paths_up_to m = 
    (toInteger . length) $ filter 
        is_int
        [calc_min_path w l h 
            | w <- [1..m]
            , l <- [w..m]
            , h <- [l..m]]

add_new_m :: Integer -> Integer -> Integer
add_new_m m m_minus_one_value = 
    m_minus_one_value + ((toInteger . length) $ filter is_int min_paths)
    where min_paths = [calc_min_path w l h | w <- [1..m], l <- [w..m], h <- [m]]

subtract_new_m :: Integer -> Integer -> Integer
subtract_new_m m m_plus_one_value = 
    m_plus_one_value - ((toInteger . length) $ filter is_int min_paths)
    where min_paths = [calc_min_path w l h | w <- [1..(m+1)], l <- [w..(m+1)], h <- [m+1]]

add_new_m_pow_2 :: Integer -> Integer
add_new_m_pow_2 m = 
    (toInteger . length) $ filter is_int min_paths
    where min_paths = [calc_min_path w l h | h <- [((m `div` 2) + 1)..m], w <- [1..h], l <- [w..h]]

add_new_m_given_lower_m :: Integer -> Integer -> Integer
add_new_m_given_lower_m m lower_m = 
    (toInteger . length) $ filter is_int min_paths
    where min_paths = [calc_min_path w l h | h <- [(lower_m + 1)..m], w <- [1..h], l <- [w..h]]

bsearch :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
bsearch lb m ub bound lb_num_paths =
    case n_paths `compare` bound of
        LT -> case n_paths_succ `compare` bound of
            LT -> m : (bsearch (m+1) case_lt_m ub bound n_paths_succ)
            EQ -> [m + 2]
            GT -> [m + 1]
        EQ -> [m + 1]
        GT -> m : (bsearch lb case_gt_m m bound lb_num_paths)
    where n_paths = lb_num_paths + (add_new_m_given_lower_m m lb)
          n_paths_succ = (add_new_m      (m+1) n_paths)
          case_lt_m = (m  + ub) `div` 2
          case_gt_m = (lb +  m) `div` 2

initial_ms = map (2^) [0..]

n_paths = scanl1 (+) $ map add_new_m_pow_2 initial_ms

n_paths_with_ms = zipWith (\m -> \x -> (m, x)) initial_ms n_paths

bound = 1000000

--first_m_over = (\(Just x) -> fst x) (Data.List.find
--    (\(m, x) -> x > bound)
--    n_paths_with_ms)

first_m_over = 2048 -- calculated via above formula

ub = first_m_over
lb = first_m_over `div` 2
curr_m = (ub + lb) `div` 2

sought = bsearch lb curr_m ub bound (calc_n_int_min_paths_up_to lb)

main = do
	(putStrLn . show) sought