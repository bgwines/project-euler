{- The radical of n, rad(n), is the product of the distinct prime factors of n. For example, 504 = 2^3 × 3^2 × 7, so rad(504) = 2 × 3 × 7 = 42.

If we calculate rad(n) for 1 ≤ n ≤ 10, then sort them on rad(n), and sorting on n if the radical values are equal, we get:

	< table >

Let E(k) be the kth element in the sorted n column; for example, E(4) = 8 and E(6) = 9.

If rad(n) is sorted for 1 ≤ n ≤ 100000, find E(10000). -}

import qualified Zora.Math as ZMath
import qualified Zora.List as ZList

import qualified Data.Ord as Ord
import qualified Data.List as List

ub :: Integer
ub = 100000

rad :: Integer -> Integer
rad = product 
	. ZList.uniqueify
	. ZMath.factor

ns_and_rads :: [(Integer, Integer)]
ns_and_rads
	= List.sortBy (Ord.comparing snd)
	. ZList.map_keep rad
	$ [1..ub]

e :: Integer -> Integer
e i = fst $ ns_and_rads !! (fromInteger $ i - 1)

main :: IO ()
main = do
	putStrLn . show $ e 10000