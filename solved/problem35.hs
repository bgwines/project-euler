
import Prime
import Perms

is_circular_prime p = and $ map is_prime $ map read $ gen_cycles $ show p

sought = length $ filter is_circular_prime $ takeWhile (\x -> x < 1000000) primes