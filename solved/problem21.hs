
import Euler

safeindex :: [Integer] -> Integer -> Integer
safeindex arr i
	| (fromIntegral i > (length arr) - 1) = -1
	| otherwise = arr !! (fromIntegral i)

amicable :: [Integer] -> Integer -> Bool
amicable arr i =
	((arr `safeindex` (arr !! (fromIntegral i))) == i)
	&&
	(arr `safeindex` i) /= i

factor_sums = [0] ++ map (sum . factorize) [1..10000]

amicables = filter (amicable factor_sums) [1..10000]

main = do
	putStrLn . show $ sum amicables