
import qualified Zora.Math as ZMath

appending = [3,7,9]
starting = [2,3,5,7,9]

ext_l :: Integer -> Integer -> Integer
ext_l n p = (p * 10^(length . show $ n)) + n

ext_r :: Integer -> Integer -> Integer
ext_r n p = (n * 10) + p

unext_r :: Integer -> Integer
unext_r n = floor $ (fromIntegral n) / 10

unext_l :: Integer -> Integer
unext_l n = n `mod` (10^((length . show $ n) - 1))

truncatable :: (Integer -> Integer) -> Integer -> Bool
truncatable unext n =
	if n' == 0
		then ZMath.prime n
		else (ZMath.prime n') && (truncatable unext n')
		where
			n' :: Integer
			n' = unext n

l_truncatable :: Integer -> Bool
l_truncatable = truncatable unext_l

r_truncatable :: Integer -> Bool
r_truncatable = truncatable unext_r

truncatable_both_directions :: Integer -> Bool
truncatable_both_directions n = (r_truncatable n) && (l_truncatable n)

truncatable_primes :: [Integer]
truncatable_primes
	= take 11
	. filter truncatable_both_directions
	. drop 4
	$ ZMath.primes

main :: IO ()
main = do
	putStrLn . show $ truncatable_primes
