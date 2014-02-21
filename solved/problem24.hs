
import Euler

fact n = product [1..n]

f const_e e len =
    case len of
        0 -> []
        _ -> e : f const_e (e + const_e) (len-1)
f1 e len = f e e len

splice_out i l =
    let a = fst $ splitAt i l 
        b = snd $ splitAt i l 
    in  a ++ tail b

calc_splice_index src desired_index = 
    let prefix_sums = f1 (fact $ length src - 1) $ length src
    in length
        (takeWhile
            (\x -> x <= desired_index)
            prefix_sums
        )

calc_updated_index desired_index splice_index src = 
	desired_index - (splice_index) * (fact $ length src - 1)

get_splice_pair i l = (l !! i, splice_out i l)

desired_index = 15
src = [1..5]

rec src desired_index
    | length src == 1 = src
    | otherwise = 
    	let splice_index = calc_splice_index src desired_index
            spliced_arr = splice_out splice_index src
            updated_index = calc_updated_index desired_index splice_index src
        in
           (src !! splice_index)
           :
           rec spliced_arr updated_index

