{-The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:

1! + 4! + 5! = 1 + 24 + 120 = 145

Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169; it turns out that there are only three such loops that exist:

169 -> 363601 -> 1454 -> 169
871 -> 45361 -> 871
872 -> 45362 -> 872

It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,

69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)
78 -> 45360 -> 871 -> 45361 (-> 871)
540 -> 145 (-> 145)

Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain with a starting number below one million is sixty terms.

How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?-}

import Data.Char

cyclers = [169, 363601, 1454, 169, 145, 871, 872]

cyclelength 169 = 4
cyclelength 363601 = 4
cyclelength 1454 = 4

cyclelength 145 = 1

cyclelength 872 = 2
cyclelength 45362 = 2

cyclelength 871 = 2
cyclelength 45361 = 2

fact :: Integer -> Integer
fact n = product [1..n]

next :: Integer -> Integer
next n = sum $ map fact $ map fromIntegral $ map digitToInt $ show n

calc_chain :: Integer -> [Integer] 
calc_chain n = calc_chain_rec n []

calc_chain_rec :: Integer -> [Integer] -> [Integer]
calc_chain_rec n chain
    | (invalid n) = []
    | (n `elem` cyclers) =
        chain ++ [n]
    | otherwise =
        calc_chain_rec (next n) (chain ++ [n])

calc_chain_length :: Integer -> Integer
calc_chain_length n =
    let chain = calc_chain n in
        case chain of [] -> 0
                      _ -> (fromIntegral $ length chain) + (cyclelength $ last chain) - 2

invalid :: Integer -> Bool
invalid n = (n == 2) || (n == 1) || (n == 40585)

l = map (\n -> (n, calc_chain_length n)) [3..1000000]
length_60 = map (\(a,b) -> a) $ filter (\(a,b) -> b == 60) $ l