
import Prime

appending = [3,7,9]
starting = [2,3,5,7,9]
--r_s = [1,3,5,7,9]

ext_l :: Integer -> Integer -> Integer
ext_l n p = (p * 10^(length $ show n)) + n

ext_r :: Integer -> Integer -> Integer
ext_r n p = n * 10 + p

unext_r :: Integer -> Integer
unext_r n = floor $ (fromIntegral n) / 10

unext_l :: Integer -> Integer
unext_l n = n `mod` 10^((length $ show n) - 1)

is_l_truncatable :: Integer -> Bool
is_l_truncatable n
    | (unext_l n == 0) = is_prime n
    | otherwise = (is_prime $ unext_l n) && (is_l_truncatable $ unext_l n)

is_r_truncatable :: Integer -> Bool
is_r_truncatable n
    | (unext_r n == 0) = is_prime n
    | otherwise = (is_prime $ unext_r n) && (is_r_truncatable $ unext_r n)

both :: Integer -> Bool
both n = (is_r_truncatable n) && (is_l_truncatable n)

truncatable_primes :: [Integer]
truncatable_primes =
    take 11 $ filter both $ drop 4 primes