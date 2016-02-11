{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Ord as Ord
import qualified Data.List as List
import qualified Zora.Math as ZMath

quad_formula :: forall a. (Floating a) => Integer -> [a]
quad_formula m = [qf (-), qf (+)]
    where
        qf :: (a -> a -> a) -> a
        qf op = (1 / 6) * (op (fromInteger 1) (sqrt $ fromInteger (24 * m + 1)))

is_pentagonal :: Integer -> Bool
is_pentagonal = any (\m -> (ZMath.is_int m) && (m >= 0)) . quad_formula

diff_and_sum_pentagonal :: (Integer, Integer) -> Bool
diff_and_sum_pentagonal (a, b) = and
    [ is_pentagonal (b - a)
    , is_pentagonal (b + a) ]

pentagonals :: [Integer]
pentagonals = map pent [1..5000]
    where
        pent :: Integer -> Integer
        pent n = div (n * ((3 * n) - 1)) 2

valid_differences :: [(Integer, Integer)]
valid_differences
    = filter diff_and_sum_pentagonal
    . concatMap f . List.tails -- pairs of elems where fst is < snd
    $ pentagonals
    where
        f :: [Integer] -> [(Integer, Integer)]
        f [] = []
        f (x:[]) = []
        f (x:xs) = map (x, ) xs

main :: IO ()
main = do
    print $ minimum . map d $ valid_differences
    where
        d :: (Integer, Integer) -> Integer
        d = uncurry (flip (-))