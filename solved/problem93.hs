{-
By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the four arithmetic operations (+, −, *, /) and brackets/parentheses, it is possible to form different positive integer targets.

For example,

8 = (4 * (1 + 3)) / 2
14 = 4 * (3 + 1 / 2)
19 = 4 * (2 + 3) − 1
36 = 3 * 4 * (2 + 1)

Note that concatenations of the digits, like 12 + 34, are not allowed.

Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 is the maximum, and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.

Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive integers, 1 to n, can be obtained, giving your answer as a string: abcd.
-}

import Euler
import qualified Data.List
import qualified Data.Ord

ops = [(-), (+), (*), (/)]

get_numbers_hit_rec :: [Double] -> [Double]
get_numbers_hit_rec l
	| (length l) == 2 =
		[e  `op` e' | op <- ops] ++ 
		[e' `op` e  | op <- ops]
	| otherwise = 
		left_associative_rec ++ right_associative_rec
		where
			e  = head l
			e' = last l
			rec  = get_numbers_hit_rec $ tail l
			rec' = get_numbers_hit_rec $ init l
			left_associative_rec  = [e  `op` r  | op <- ops, r <- rec]
			right_associative_rec = [r' `op` e' | op <- ops, r' <- rec']


-- uniqueify sorts
get_numbers_hit_sorted :: [Double] -> [Integer]
get_numbers_hit_sorted =
	map format . uniqueify . filter valid . concat . map get_numbers_hit_rec . gen_perms
	where
		valid x = (x > 0) && (is_int x) && (show x /= "Infinity")
		format = (toInteger . round)

sets_containing_max :: Double -> [[Double]]
sets_containing_max n = 
	map (n :) [[a, b, c]
		| a <- [1..n]
		, b <- [1..n]
		, c <- [1..n]
		, a <= b
		, b <= c]

get_highest_consecutive_number_hit :: [Double] -> Integer
get_highest_consecutive_number_hit set =
	length'
		$ takeWhile
			pair_eq
			$ zipWith pair [1..] sorted_numbers_hit
	where
		sorted_numbers_hit = Data.List.sort $ get_numbers_hit_sorted set
		pair_eq (a, b) = (a == b)

find_running_bests_rec :: [([Integer], Integer)] -> ([Integer], Integer) -> [([Integer], Integer)]
find_running_bests_rec [] so_far = []
find_running_bests_rec l so_far = 
	case (snd so_far) `compare` (snd e) of
		LT -> e : (find_running_bests_rec (tail l) e)
		EQ -> e : (find_running_bests_rec (tail l) e)
		GT ->      find_running_bests_rec (tail l) so_far
	where e = head l

find_running_bests :: [([Integer], Integer)] -> [([Integer], Integer)]
find_running_bests [] = []
find_running_bests l = (head l) : find_running_bests_rec (tail l) (head l)

values :: [([Integer], Integer)]
values = map
	format
	$ map_keep
		get_highest_consecutive_number_hit
		$ concat $ map sets_containing_max [1..]
	where format (l, n) = (map to_int l, n)

main = do
	(putStrLn . show) $ find_running_bests values