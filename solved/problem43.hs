import Data.Char
import Data.List

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

get_splice_pair i l = (l !! i, splice_out i l)

splice_out i l =
    let a = fst $ splitAt i l 
        b = snd $ splitAt i l 
    in  a ++ tail b

prep l = map intToDigit $ map fromIntegral l

substr i len s = take len $ drop i s

has_property s =
	((read $ substr 1 3 s) `mod` 2) == 0 && 
	((read $ substr 2 3 s) `mod` 3) == 0 &&
	((read $ substr 3 3 s) `mod` 5) == 0 &&
	((read $ substr 4 3 s) `mod` 7) == 0 &&
	((read $ substr 5 3 s) `mod` 11) == 0 &&
	((read $ substr 6 3 s) `mod` 13) == 0 &&
	((read $ substr 7 3 s) `mod` 17) == 0

pandigitals = gen_perms [0..9]

sought = filter (has_property . prep) pandigitals

ints = map read $ map prep sought :: [Int]