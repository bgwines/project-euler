
import Prime
import Data.List

is_int :: (Show a) => a -> Bool
is_int n = ('0' == (last $ show n)) && ('.' == (last $ init $ show n))

is_square :: Integer -> Bool
is_square n = is_int $ sqrt (fromIntegral n)

is_sum_of_p_and_twice_a_square :: Integer -> Integer -> Bool
is_sum_of_p_and_twice_a_square n p = is_square $ div (n-p) 2

is_sum_of_prime_and_twice_a_square_wrapped :: Integer -> Maybe Integer
is_sum_of_prime_and_twice_a_square_wrapped n = 
	find (is_sum_of_p_and_twice_a_square n) smallprimes
	where smallprimes = filter (\p -> even (n-p)) $ takeWhile (<n) primes

is_sum_of_prime_and_twice_a_square :: Integer -> Bool
is_sum_of_prime_and_twice_a_square n =
	Nothing /= (is_sum_of_prime_and_twice_a_square_wrapped n)

odds_composites :: [Integer]
odds_composites = filter (not . is_prime) $ filter (< 5779) [5,7..]

sought :: Maybe Integer
sought = find 
	(not . is_sum_of_prime_and_twice_a_square)
	odds_composites 