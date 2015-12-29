-- | working n^2 solution (max 7^5)

import qualified Data.List as L
import qualified Data.MemoCombinators as Memo

type Point = (Integer, Integer)

n :: Integer
n = 6^5 --(30^5; 24 million)

-- | Whether `p1` is topologically less than `p2`
pt_less :: Point -> Point -> Bool
pt_less p1@(x1, y1) p2@(x2, y2) = (x1 <= x2 && y1 <= y2) && (p1 /= p2)

-- | 48 million long for n = 30^5
stations :: [Point]
stations = L.nub [((2^i) `mod` n, (3^i) `mod` n) | i <- [0 .. (2 * n)]]

max_length_path_to :: Point -> Integer
max_length_path_to = Memo.pair Memo.integral Memo.integral max_length_path_to'
    where
        max_length_path_to' :: Point -> Integer
        max_length_path_to' station
            = succ
            . maximum
            . (:) 0 -- in case of empty list
            . map max_length_path_to
            $ prev_stations
            where
                prev_stations :: [Point]
                prev_stations = filter (flip pt_less $ station) stations

main :: IO ()
main = do
    print $ maximum . map max_length_path_to $ stations