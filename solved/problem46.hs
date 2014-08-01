
import qualified Data.List as List
import qualified Zora.Math as ZMath

import Data.Maybe

is_int :: (Show a) => a -> Bool
is_int n = and 
	[ '0' == (last . show $ n)
	, '.' == (last . init . show $ n) ]

is_sum_of_p_and_twice_a_square :: Integer -> Integer -> Bool
is_sum_of_p_and_twice_a_square n p = ZMath.square $ div (n - p) 2

is_sum_of_a_prime_and_twice_a_square :: Integer -> Bool
is_sum_of_a_prime_and_twice_a_square n = 
	isJust . List.find (is_sum_of_p_and_twice_a_square n) $smallprimes
	where
		smallprimes :: [Integer]
		smallprimes
			= filter (\p -> even (n - p))
			. takeWhile (< n)
			$ ZMath.primes

odds_composites :: [Integer]
odds_composites
	= filter (not . ZMath.prime)
	. filter (< 5779)
	$ [5,7..]

sought :: Maybe Integer
sought = List.find (not . is_sum_of_a_prime_and_twice_a_square) odds_composites 

main :: IO ()
main = do
	putStrLn . show $ fromJust sought