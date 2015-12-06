
{- We shall define a square lamina to be a square outline with a square "hole" so that the shape possesses vertical and horizontal symmetry. For example, using exactly thirty-two square tiles we can form two different square laminae:

With one-hundred tiles, and not necessarily using all of the tiles at one time, it is possible to form forty-one different square laminae.

Using up to one million tiles how many different square laminae can be formed? -}

import qualified Data.Ord as Ord
import qualified Data.List as List

import Data.Maybe

t_ub :: Integer
t_ub = 5000000

-- list of outlines represented as number of tiles
valid_ts :: [Integer]
valid_ts = concatMap f squares
	where
		f :: Integer -> [Integer]
		f outer
			= map ((-) outer)
			. filter (same_parity outer)
			$ inners'
			where
				inners' :: [Integer]
				inners' = map (^2) [min_inner_diameter..(sqrt_outer)]

				sqrt_outer :: Integer
				sqrt_outer = integer_sqrt outer

				min_inner_diameter :: Integer
				min_inner_diameter = if outer < t_ub
					then 1
					else integer_sqrt $ outer - t_ub

				same_parity :: Integer -> Integer -> Bool
				same_parity s1 s2 = even $ s1 - s2

		squares :: [Integer]
		squares = map (^2) [1..outer_ub]
			where
				outer_ub :: Integer
				outer_ub = (t_ub `div` 4) + 1

integer_sqrt :: Integer -> Integer
integer_sqrt = toInteger . ceiling . sqrt . fromInteger

sought :: Int
sought
	= length
	. filter (/= 0)
	. filter (<= 1000000)
	$ valid_ts

main :: IO ()
main = do
	putStrLn . show $ sought
