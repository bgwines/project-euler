
import qualified Zora.List as ZList
import qualified Zora.Math as ZMath

is_circular_prime :: Integer -> Bool
is_circular_prime
	= all ZMath.prime
	. map read
	. ZList.cycles
	. show

num_small_circular_primes :: Integer
num_small_circular_primes
	= ZList.count is_circular_prime
	. takeWhile (< 1000000)
	$ ZMath.primes

main :: IO ()
main = do
	putStrLn . show $ num_small_circular_primes