

{-
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
-}

min_len = 1
desired_len = 5

safe_index :: (Num a) => [a] -> Int -> a
safe_index arr i =
    case i of 0 -> 0
              1 -> 1
              _ -> arr !! i

calc_num_tilings :: (Num a) => Int -> Int -> [a] -> a
calc_num_tilings n_tiles_before_placement placement_len dp_arr =
    let a_len = max 0 $ (-1) + n_tiles_before_placement
        b_len = max 0 $ (-1) + desired_len - n_tiles_before_placement - placement_len
    in case a_len + b_len of 0 -> 1
                             _ -> (safe_index dp_arr a_len) + (safe_index dp_arr b_len)

calc_num_tilings_in_arr_of_len :: (Num a) => Int -> [a] -> a
calc_num_tilings_in_arr_of_len placement_len dp_arr =
    sum
        [
            calc_num_tilings
                n_tiles_before_placement
                placement_len
                dp_arr
            | curr_tile_len <- [min_len..desired_len]
        ]

arr =
    [
        calc_num_tilings_in_arr_of_len curr_tile_len arr
        | n_tiles_before_placement <- [0..desired_len-placement_len]
    ]

sought = sum arr