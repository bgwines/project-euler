module List
( diff
, maximum_with_index
, substr
, gen_perms
, merge
, mergesort
, splice_out
, get_splice_pair
, is_palindrome
, fill_set
, fst3
, snd3
, trd3
) where

import qualified Data.List
import qualified Data.Ord
import qualified Data.Set as Set

fst3 :: (a, a, a) -> a
fst3 (a, b, c) = a

snd3 :: (a, a, a) -> a
snd3 (a, b, c) = b

trd3 :: (a, a, a) -> a
trd3 (a, b, c) = c

fill_set :: [Integer] -> Set.Set Integer
fill_set l = fill_set_rec Set.empty l

fill_set_rec :: Set.Set Integer -> [Integer] -> Set.Set Integer
fill_set_rec s [] = s
fill_set_rec s l = Set.insert (head l) (fill_set_rec s $ tail l)


is_palindrome :: (Eq e) => [e] -> Bool
is_palindrome s
    | length s <= 1 = True
    | otherwise =
        (head s == last s)
        &&
        (is_palindrome $ tail $ init s)

get_splice_pair :: Integer -> [a] -> (a, [a])
get_splice_pair i l = (l !! (fromInteger i), splice_out i l)

splice_out :: Integer -> [a] -> [a]
splice_out i l =
    let a = fst $ splitAt (fromInteger i) l 
        b = snd $ splitAt (fromInteger i) l 
    in  a ++ tail b

gen_perms :: [a] -> [[a]]
gen_perms l
    | (length l <= 1) = [l]
    | otherwise = 
        let splice_pairs = [get_splice_pair (toInteger i) l | i <- [0..((length l) - 1)]]
        in
        concat [
            [fst splice_pair : recpair | recpair <- gen_perms $ snd splice_pair]
            | splice_pair <- splice_pairs
        ]

substr :: Integer -> Integer -> [a] -> [a]
substr i len s = take (fromInteger len) $ drop (fromInteger i) s

maximum_with_index :: (Ord a) => [a] -> (a, Integer)
maximum_with_index xs =
    Data.List.maximumBy (Data.Ord.comparing fst) (zip xs [0..])

--merge :: (Ord a) => [a] -> [a] -> [a]
--merge xs@(x:xt) ys@(y:yt) = 
--  case compare x y of
--    LT -> x : (merge xt ys)
--    EQ -> x : (merge xt yt)
--    GT -> y : (merge xs yt)

mergesort :: (Ord a) => [a] -> [a]
mergesort l
    | length l == 0 || length l == 1 = l
    | otherwise = 
        let a = fst $ splitAt (floor ((fromIntegral $ length l)/2)) l
            b = snd $ splitAt (floor ((fromIntegral $ length l)/2)) l
        in merge (mergesort a) (mergesort b)

merge :: (Ord a) => [a] -> [a] -> [a]
merge a b
    | length a == 0 = b
    | length b == 0 = a
    | otherwise = case (head a < head b) of
                    True  -> head a : merge (tail a) b
                    False -> head b : merge a (tail b)

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
    case compare x y of
        LT -> x : (diff xt ys)
        EQ -> diff xt yt
        GT -> diff xs yt