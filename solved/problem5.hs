{-
2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-}

import qualified Data.List as List
import qualified Zora.Math as Math

import Data.Maybe

divisible_for_all_in_list :: [Integer] -> Integer -> Bool
divisible_for_all_in_list l n = all (\k -> n `mod` k == 0) l

-- 1..10 all have multiples in l
l :: [Integer]
l = [20,19..11]

product_of_primes :: Integer
product_of_primes = product . filter Math.prime $ l

products_of_primes :: [Integer]
products_of_primes = map ((*) product_of_primes) [1..]

divider :: Maybe Integer
divider = List.find (divisible_for_all_in_list l) products_of_primes

main :: IO ()
main = do
	putStrLn . show $ fromJust divider