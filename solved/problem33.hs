
import Data.Char

cancel :: (Integer, Integer) -> (Integer, Integer)
cancel (a, b) = (aa, bb)
    where aa = f a b
          bb = f b a

f a b = case ((show a) !! 0 == (show a) !! 1) of
    True -> div a 10
    False -> ff a b

ff a b = (fromIntegral . digitToInt . head) [d | d <- (show a), not (d `elem` (show b))]

get_actual_and_canceled :: (Integer, Integer) -> ((Integer, Integer), (Integer, Integer))
get_actual_and_canceled (a, b) = ((a, b), cancel (a, b))

flip_digits :: Integer -> String
flip_digits b = show b !! 1 : [(show b !! 0)]

is_valid_starting_frac :: (Integer, Integer) -> Bool
is_valid_starting_frac (a, b) = 
	not ((a `mod` 10 == 0) && (b `mod` 10 == 0))
	&&
	0 /= length [d | d <- (show a), d `elem` (show b)]
	&&
	(show a) /= flip_digits b
	&& a < b

fractions :: [(Integer, Integer)]
fractions = filter is_valid_starting_frac [(a, b) | a <- [10..99], b <- [10..99]]

eval_frac :: (Integer, Integer) -> Double
eval_frac (a, b) = (fromInteger a) / (fromInteger b)

is_validly_cancelling_fraction :: (Integer, Integer) -> Bool
is_validly_cancelling_fraction frac = (eval_frac frac) == ((eval_frac . cancel) frac)

valid_fractions :: [(Integer, Integer)]
valid_fractions = filter is_validly_cancelling_fraction fractions