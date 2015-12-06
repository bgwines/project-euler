{- Using all of the digits 1 through 9 and concatenating them freely to form decimal integers, different sets can be formed. Interestingly with the set {2,5,47,89,631}, all of the elements belonging to it are prime.

How many distinct sets containing each of the digits one through nine exactly once contain only prime elements? -}

{-# LANGUAGE ScopedTypeVariables #-}

import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Data.Numbers.Primes as Primes

import qualified Data.MemoCombinators as Memo

import Data.Maybe

import Control.Applicative

prime_sets :: [[Integer]]
prime_sets
	= filter have_all_digits
	. gen_prime_sets
	$ [1..9]

have_all_digits :: [Integer] -> Bool
have_all_digits
	= (==) (concatMap show [1..9])
	. List.sort
	. concatMap show

gen_prime_sets :: [Integer] -> [[Integer]]
gen_prime_sets = Memo.list Memo.integral
	( ZList.uniqueify
	. map List.sort
	. gen_prime_sets' 0)

gen_prime_sets' :: Integer -> [Integer] -> [[Integer]]
gen_prime_sets' so_far digits
	= (:) []
	. filter ((/=) [])
	$ stop ++ continue
	where
		stop :: [[Integer]]
		stop = if ZMath.prime_miller_rabin so_far
			then map ((:) so_far) $ gen_prime_sets digits
			else [[]]

		continue :: [[Integer]]
		continue = concat
			[ gen_prime_sets'
				(cat so_far (digits !! i))
				(ZList.remove_at_index (fromIntegral $ i) digits)
			| i <- [0..(length digits - 1)] ]

		cat :: Integer -> Integer -> Integer
		cat a b = (a * 10) + b

main :: IO ()
main = do
	putStrLn . show $ length prime_sets
