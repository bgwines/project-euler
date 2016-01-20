{- Diophantine equation:

        x   x-1   1
        - * --- = -
        n   n-1   2

    solutions generated by `next -}

import qualified Data.List as List

import Data.Maybe

next :: (Integer, Integer) -> (Integer, Integer)
next (x, n) =
    ( 3*x + 2*n - 2
    , 4*x + 3*n - 3 )

findFirstSatisfyingElem :: [(Integer, Integer)] -> Integer
findFirstSatisfyingElem = fst . fromJust . List.find ((> (10^12)) . snd)

main :: IO ()
main = print $ findFirstSatisfyingElem . iterate next $ (15, 21)