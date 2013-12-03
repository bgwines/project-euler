{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

factors :: (Num a, Integral a) => a -> a -> Bool
factors k n
	| 0 == (mod n k) = True
	| otherwise = False

is_prime_rec :: (Num a, Integral a) => a -> a -> Bool
is_prime_rec n k
	| n == 1 = False
	| k < 2 = True
	| ((n `mod` k) == 0) = False
	| otherwise = is_prime_rec n (k-1)

is_prime :: (Num a, Integral a) => a -> Bool
is_prime n = is_prime_rec n (n-1)

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt

primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
nonprimes = foldr1 f $ map g $ tail primes
  where 
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

n = 600851475143
prime_factors = filter (\x -> factors x n) primes