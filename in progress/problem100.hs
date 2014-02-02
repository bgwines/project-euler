{-

	(x/(x+y)) * ((x-1)/(x+y-1)) = 1/2

		=>

		y^2 + (2x - 1)y + (-x^2 - x + 2) = 0

		solutions: (1/2 - x) +- sqrt(8x^2 - 7)
-}

import qualified Data.List

is_positive_int :: (RealFrac a) => a -> Bool
is_positive_int n = 
	n == (fromInteger $ round n)
	&&
	n >= 0

from_just :: Maybe a -> a
from_just (Just x) = x

calc_ys :: Double -> Maybe (Double, Double)
calc_ys x = 
	case determinant >= 0 of
		True -> Just (a + b, a - b)
		False -> Nothing
	where
		determinant = (8 * x^2) - (8 * x) + 1
		a = (0.5 - x)
		b = (0.5 * (sqrt determinant))

calc_int_y :: Integer -> Maybe Double
calc_int_y x = 
	case (y_pair_maybe == Nothing) of
		True -> Nothing
		False ->
			case (is_positive_int $ fst y_pair) of
				True -> Just (fst y_pair)
				False -> case (is_positive_int $ snd y_pair) of
					True -> Just (snd y_pair)
					False -> Nothing
	where
		y_pair_maybe = calc_ys (fromIntegral x)
		y_pair = from_just y_pair_maybe

ub = 1000000000000.0
lb = round $ ub / 1.5--1.2--4--14213

test_pairs :: [(Integer, Maybe Double)]
test_pairs = 
	filter
		(\(x, y) -> y /= Nothing) 
		$ map 
			(\x -> (x, calc_int_y x)) 
			[lb..]
	
pair_diffs :: [((Integer, Double), Double)]
pair_diffs = map f test_pairs
	where
		f = (\(x, y) -> ((x, from_just y), frac x y))
		frac x y = (from_just y) / (fromIntegral x)


pairs :: [(Integer, Maybe Double)]
pairs = map (\x -> (x, calc_int_y x)) [lb..]

--sought :: Maybe (Integer, Maybe Double)
sought = map 
	format
	$ filter satisfactory pairs
	where
		satisfactory (x, y) = 
			(y /= Nothing) && ((fromIntegral x) + (from_just y) > ub)
		format (x, y) =
			(x, from_just y, (fromIntegral x) + (from_just y))

