{-
Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 21 elements in this set.

How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?-}

import qualified Data.Numbers.Primes as Primes
import qualified Data.List
import qualified Data.MemoCombinators as Memo

smallest_prime_divisor :: Integer -> Integer
smallest_prime_divisor = Memo.integral smallest_prime_divisor'
	where
		smallest_prime_divisor' 1 = 1
		smallest_prime_divisor' n = 
			head $ filter (\p -> (n `mod` p) == 0) Primes.primes

euler_phi :: Integer -> Integer
euler_phi = Memo.integral euler_phi'
	where
		euler_phi' 1 = 1
		euler_phi' n = euler_phi n' *
			if (p' == p) then p else (p-1)
			where
				p  = smallest_prime_divisor n
				n' = n `div` p
				p' = smallest_prime_divisor n'

----------------------------------------------------------------------
----------------------------------------------------------------------

from_just (Just x) = x

factorize :: Integer -> [Integer]
factorize = Memo.integral factorize'
    where
        factorize' 1 = []
        factorize' n = p : factorize (n `div` p)
            where p = from_just $ Data.List.find (\p -> (n `mod` p) == 0) Primes.primes

euler_phi_for_powers_of_primes :: (Integer, Integer) -> Integer
euler_phi_for_powers_of_primes (p, a) = p^(a-1) * (p-1)

-- ultramegafast
euler_phi_prev :: Integer -> Integer
euler_phi_prev 1 = 0
euler_phi_prev n = product 
    $ map
        euler_phi_for_powers_of_primes
        $ map format $ Data.List.group $ factorize n
    where
        format l = (head l, (toInteger . length) l) 


x = 32674

main = do
	(putStrLn . show) $ sum $ map euler_phi_prev [2..x]
