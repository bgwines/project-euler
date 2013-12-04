{-
Triangle	 	P3,n=n(n+1)/2	 	1, 3, 6, 10, 15, ...
Square	 	P4,n=n2	 	1, 4, 9, 16, 25, ...
Pentagonal	 	P5,n=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
Hexagonal	 	P6,n=n(2n−1)	 	1, 6, 15, 28, 45, ...
Heptagonal	 	P7,n=n(5n−3)/2	 	1, 7, 18, 34, 55, ...
Octagonal	 	P8,n=n(3n−2)	 	1, 8, 21, 40, 65, ...
-}

import qualified Data.Set as Set
import List

triangle :: Integer -> Integer
triangle n = (n * (1*n + 1)) `div` 2
square   n = (n * (1*n + 0))
pentagon n = (n * (3*n - 1)) `div` 2
hexagon  n = (n * (2*n - 1))
heptagon n = (n * (5*n - 3)) `div` 2
octagon  n = (n * (3*n - 2))

triangulars :: [Integer]
triangulars = map triangle [1..]
squares     = map square   [1..]
pentagonals = map pentagon [1..]
hexagonals  = map hexagon  [1..]
heptagonals = map heptagon [1..]
octagonals  = map octagon  [1..]

four_digit_triangulars :: [Integer]
four_digit_triangulars = get_only_4_digits_elems triangulars
four_digit_squares     = get_only_4_digits_elems squares
four_digit_pentagonals = get_only_4_digits_elems pentagonals
four_digit_hexagonals  = get_only_4_digits_elems hexagonals
four_digit_heptagonals = get_only_4_digits_elems heptagonals
four_digit_octagonals  = get_only_4_digits_elems octagonals

four_digit_triangulars_with_sourceNs :: [(Integer, Integer)]
four_digit_triangulars_with_sourceNs = zipWith tupleify [1..] four_digit_triangulars
four_digit_squares_with_sourceNs     = zipWith tupleify [1..] four_digit_squares
four_digit_pentagonals_with_sourceNs = zipWith tupleify [1..] four_digit_pentagonals
four_digit_hexagonals_with_sourceNs  = zipWith tupleify [1..] four_digit_hexagonals
four_digit_heptagonals_with_sourceNs = zipWith tupleify [1..] four_digit_heptagonals
four_digit_octagonals_with_sourceNs  = zipWith tupleify [1..] four_digit_octagonals

tupleify :: a -> b -> (a, b)
tupleify a b = (a, b)

get_only_4_digits_elems :: [Integer] -> [Integer]
get_only_4_digits_elems l = filter (f (==)) $ takeWhile (f (<=)) l
	where f op n = op (ceiling $ logBase 10 (fromInteger n)) 4

is_cycle_pair :: Integer -> Integer -> Bool
is_cycle_pair a b = (drop 2 $ show a) == (take 2 $ show b)

is_cycle_this_permutation :: [Integer] -> Bool
is_cycle_this_permutation l = 
	(and $ zipWith is_cycle_pair (l) (tail l))
	&&
	(is_cycle_pair (last l) (head l))

is_cycle :: [Integer] -> Bool
is_cycle l = or $ map is_cycle_this_permutation unique_mod_cycling_perms
	where unique_mod_cycling_perms = map (head l :) (gen_perms $ tail l)

all_different :: (Ord a) => [a] -> Bool
all_different l = (length l) == (Set.size $ Set.fromList l)

find_only_valid_cycle_of_length_6 :: [Integer]
find_only_valid_cycle_of_length_6 = ((map snd) . head)
	[[e3, e4, e5, e6, e7, e8] |
		e3 <- four_digit_triangulars_with_sourceNs,
		e4 <- four_digit_squares_with_sourceNs,
		e5 <- four_digit_pentagonals_with_sourceNs,
		e6 <- four_digit_hexagonals_with_sourceNs,
		e7 <- four_digit_heptagonals_with_sourceNs,
		e8 <- four_digit_octagonals_with_sourceNs,
		is_cycle      $ map snd [e3, e4, e5, e6, e7, e8],
		all_different $ map fst [e3, e4, e5, e6, e7, e8]]

main = do
	(putStrLn . show) $ sum find_only_valid_cycle_of_length_6