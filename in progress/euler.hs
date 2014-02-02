module Euler
( diff
, gen_perms
, gen_cycles
, powerset
, merge
, mergesort
, factorize
, euler_phi
, is_power
, get_num_digits
, find
, find_guaranteed
, maximum_with_index
, substr
, splice_out
, get_splice_pair
, is_palindrome
, fill_set
, fst3
, snd3
, trd3
, fromRealFrac
, from_just
) where

import qualified Data.List
import qualified Data.Ord
import qualified Data.Set as Set
import qualified Data.MemoCombinators as Memo
import Prime

cycle_list :: [a] -> [a]
cycle_list l = (tail l) ++ [head l]

gen_cycles :: (Eq a) => [a] -> [[a]]
gen_cycles l = gen_cycles_rec l $ cycle_list l

gen_cycles_rec :: (Eq a) => [a] -> [a] -> [[a]]
gen_cycles_rec original_l l
    | l == original_l = [l]
    | otherwise = [l] ++ (gen_cycles_rec original_l $ cycle_list l)

is_power :: Integer -> Integer -> Bool
is_power n e = (round (fromIntegral n ** (1/(fromInteger e))))^e == n

get_num_digits :: Integer -> Integer
get_num_digits n = (1 + (floor $ logBase 10 (fromInteger n)))

divides_both :: Integer -> Integer -> Integer -> Bool
divides_both d n m = (n `mod` d == 0) && (m `mod` d == 0)

euler_phi_rec :: Integer -> Integer -> Integer -> Integer
euler_phi_rec n m count
    | (n == m) = count
    | (coprime n m) = euler_phi_rec n (m+1) (count+1)
    | otherwise     = euler_phi_rec n (m+1) count

euler_phi_slow :: Integer -> Integer
euler_phi_slow n = euler_phi_rec n 1 0

euler_phi_medium :: Integer -> Integer
euler_phi_medium n 
    | (n == 2) = 1
    | (even n) = case (even half) of
        True  -> 2 * (euler_phi_medium half)
        False -> 1 * (euler_phi_medium half)
    | (odd n) = euler_phi_slow n
        where half = div n 2

factorize :: Integer -> [Integer]
factorize = Memo.integral factorize'
    where
        factorize' 1 = []
        factorize' n = p : factorize (n `div` p)
            where p = find_guaranteed (\p -> (n `mod` p) == 0) primes

euler_phi_for_powers_of_primes :: (Integer, Integer) -> Integer
euler_phi_for_powers_of_primes (p, a) = p^(a-1) * (p-1)

-- ultramegafast
euler_phi :: Integer -> Integer
euler_phi 1 = 0
euler_phi n = product 
    $ map
        euler_phi_for_powers_of_primes
        $ map format $ Data.List.group $ factorize n
    where
        format l = (head l, (toInteger . length) l) 

from_just :: Maybe a -> a
from_just (Just x) = x

fromRealFrac :: Integer -> Double
fromRealFrac = fromRational . toRational

find_guaranteed :: (a -> Bool) -> [a] -> a
find_guaranteed f l = 
    if f $ head l
        then head l
        else find_guaranteed f $ tail l

find :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f l = case f $ head l of
    True -> Just (head l)
    False -> find f $ tail l

powerset_rec :: [a] -> [a] -> [[a]]
powerset_rec src so_far
    | (length src == 0) = [so_far]
    | otherwise = without ++ with
    where without = powerset_rec (tail src) (so_far)
          with = powerset_rec (tail src) ((head src) : so_far)

powerset :: [a] -> [[a]]
powerset l = powerset_rec l []

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