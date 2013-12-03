factorize :: Integer -> [Integer]
factorize n
	| n == 1 = [1]
	| otherwise = filter (\m -> mod n m == 0) [1..ceiling((fromInteger n) /2)]

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

sought = sum amicables