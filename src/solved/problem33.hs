
import qualified Zora.List as ZList
import qualified Data.Char as Char

cancel :: (Integer, Integer) -> (Integer, Integer)
cancel (a, b) = (aa, bb)
    where aa = f a b
          bb = f b a

f :: Integer -> Integer -> Integer
f a b = if ((show a) !! 0) == ((show a) !! 1)
    then div a 10
    else ff a b

ff :: Integer -> Integer -> Integer
ff a b
	= fromIntegral
	. Char.digitToInt
	. head
	. filter (flip notElem $ show b)
	. show
	$ a

actual_and_canceled :: (Integer, Integer) -> ((Integer, Integer), (Integer, Integer))
actual_and_canceled pair = (pair, cancel pair)

flip_digits :: Integer -> String
flip_digits b = [show b !! 1] ++ [(show b !! 0)]

is_valid_starting_frac :: (Integer, Integer) -> Bool
is_valid_starting_frac (a, b) = and
	[ (a `mod` 10 /= 0) || (b `mod` 10 /= 0)
	, any (flip elem $ show b) . show $ a
	, (show a) /= (flip_digits b)
	, a < b ]

fractions :: [(Integer, Integer)]
fractions = filter is_valid_starting_frac [(a, b) | a <- [10..99], b <- [10..99]]

eval_frac :: (Integer, Integer) -> Double
eval_frac (a, b) = (fromInteger a) / (fromInteger b)

is_validly_cancelling_fraction :: (Integer, Integer) -> Bool
is_validly_cancelling_fraction frac
	= (eval_frac frac) == (eval_frac . cancel $ frac)

valid_fractions :: [(Integer, Integer)]
valid_fractions = filter is_validly_cancelling_fraction fractions
