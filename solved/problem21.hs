
import qualified Zora.Math as ZMath
import qualified Data.MemoCombinators as Memo

f :: Integer -> Integer
f = Memo.integral (sum . ZMath.divisors)

amicable :: Integer -> Bool
amicable n = ((f . f $ n) == n) && ((f n) /= n)

amicables :: [Integer]
amicables = filter amicable [1..10000]

main :: IO ()
main = do
	putStrLn . show $ sum amicables