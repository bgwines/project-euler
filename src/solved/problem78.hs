{-
-}

import Euler
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.MemoCombinators as Memo

pent :: Integer -> Integer
pent n = (n * (3*n - 1)) `div` 2

generalized_pents :: [Integer]
generalized_pents = map pent alternatings

-- [0, 1, -1, 2, -2, 3, -3, ...]
alternatings :: [Integer]
alternatings = concatMap (\i -> [i, -i]) [1..]

signs :: [Integer]
signs = map f alternatings
	where f i = round $ (-1)**(1 + (fromIntegral i)) --1+ to invert (-1 -> 1, 1 -> -1)

{-
p(k) = p(k − 1) + p(k − 2) − p(k − 5) − p(k − 7) + p(k − 12) + p(k − 15) − p(k − 22) − ...
where p(0) is taken to equal 1, and p(k) is taken to be zero for negative k.
-}

p' :: Integer -> Integer
p' n
	| n < 0 = 0
	| n == 0 = 1
	| otherwise = sum $ zipWith (*) signs $ map p params
		where
			params = takeWhile (>= 0) $ map ((-) n) generalized_pents

p :: Integer -> Integer
p = Memo.integral p'

main :: IO ()
main = do
	(putStrLn . show) $ find_guaranteed sought [1..]
		where
			sought n = p n `mod` 1000000 == 0
