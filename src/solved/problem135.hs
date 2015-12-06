{- Given the positive integers, x, y, and z, are consecutive terms of an arithmetic progression, the least value of the positive integer, n, for which the equation, x2 − y2 − z2 = n, has exactly two solutions is n = 27:

34^2 − 27^2 − 20^2 = 12^2 − 9^2 − 6^2 = 27

It turns out that n = 1155 is the least value which has exactly ten solutions.

How many values of n less than one million have exactly ten distinct solutions? -}

import qualified Zora.Math as ZM (divisors)
import Zora.List (snoc)

import Data.Maybe (catMaybes)

type U = Integer
type V = Integer

-- z = z; y = z + k; x = z + 2k
-- n = (3k - z)(k + z) = uv
solutions :: Integer -> [(Integer, Integer)]
solutions n
    = filter (\(k, z) -> k > 0 && z > 0)
    . catMaybes
    . map getKZ
    $ uvs
    where
        uvs :: [(U, V)]
        uvs = map (\d -> (d, n `div` d)) $ ZM.divisors n ++ [n]

        getKZ :: (U, V) -> Maybe (Integer, Integer)
        getKZ uv = combineMaybes (getK uv) (getZ uv)

        getK :: (U, V) -> Maybe Integer
        getK (u, v)
            | (((u + v) `mod` 4) == 0) = Just ((u + v) `div` 4)
            | otherwise = Nothing

        getZ :: (U, V) -> Maybe Integer
        getZ (u, v)
            | (((3*v - u) `mod` 4) == 0) = Just ((3*v - u) `div` 4)
            | otherwise = Nothing

        combineMaybes :: Maybe a -> Maybe b -> Maybe (a, b)
        combineMaybes (Just a) (Just b) = Just (a, b)
        combineMaybes _ _ = Nothing

main :: IO ()
main = do
    print $ length . filter ((==) 10 . length . solutions) $ [999999..ub]