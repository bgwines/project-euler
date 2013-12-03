
import Data.Char

fact :: Integer -> Integer
fact 0 = 1
fact n = product [1..n]

f :: Integer -> Bool
f n = n == (sum $ map fact $ map (toInteger . digitToInt) $ show n)

l = filter f [1..]
sought = sum l