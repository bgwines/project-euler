{-# LANGUAGE ScopedTypeVariables #-}

{-
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
-}

fact :: Int -> Int
fact n = product [1..n]

f :: Int -> Int -> Int -> [Int] 
f const_e e 0 = []
f const_e e len = (:) e $ f const_e (e + const_e) (len-1)

f1 :: Int -> Int -> [Int]
f1 e len = f e e len

splice_out :: forall a. Int -> [a] -> [a]
splice_out i l = x ++ ys
    where
        x :: [a]
        ys :: [a]
        (x, y:ys) = splitAt i l 

calc_splice_index :: [a] -> Int -> Int
calc_splice_index src desired_index
    = length
    . takeWhile (<= desired_index)
    $ prefix_sums
    where
        prefix_sums :: [Int]
        prefix_sums = f1 (fact $ length src - 1) $ length src

calc_updated_index :: Int -> Int -> [a] -> Int
calc_updated_index desired_index splice_index src = 
	desired_index - (splice_index * (fact . pred . length $ src))

get_splice_pair :: Int -> [a] -> (a, [a])
get_splice_pair i l = (l !! i, splice_out i l)

calc_nth_permutation :: [a] -> Int -> [a]
calc_nth_permutation src desired_index
    | length src == 1 = src
    | otherwise = 
    	let splice_index = calc_splice_index src desired_index
            spliced_arr = splice_out splice_index src
            updated_index = calc_updated_index desired_index splice_index src
        in
           (src !! splice_index)
           :
           (calc_nth_permutation spliced_arr updated_index)

main :: IO ()
main = do
    putStrLn $ concatMap show $ calc_nth_permutation [0..9] 1000000