module Perms
( gen_perms
, gen_cycles
) where

gen_perms :: [a] -> [[a]]
gen_perms l
    | (length l <= 1) = [l]
    | otherwise = 
        let splice_pairs = [get_splice_pair i l | i <- [0..((length l) - 1)]]
        in
        concat [
            [fst splice_pair : recpair | recpair <- gen_perms $ snd splice_pair]
            | splice_pair <- splice_pairs
        ]

cycle_list :: [a] -> [a]
cycle_list l = (tail l) ++ [head l]

gen_cycles :: (Eq a) => [a] -> [[a]]
gen_cycles l = gen_cycles_rec l $ cycle_list l

gen_cycles_rec :: (Eq a) => [a] -> [a] -> [[a]]
gen_cycles_rec original_l l
    | l == original_l = [l]
    | otherwise = [l] ++ (gen_cycles_rec original_l $ cycle_list l)

get_splice_pair i l = (l !! i, splice_out i l)

splice_out i l =
    let a = fst $ splitAt i l 
        b = snd $ splitAt i l 
    in  a ++ tail b