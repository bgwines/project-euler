{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
-}

-- from problem 3
is_prime_rec :: (Num a, Integral a) => a -> a -> Bool
is_prime_rec n k
	| n == 1 = False
	| k < 2 = True
	| ((n `mod` k) == 0) = False
	| otherwise = is_prime_rec n (k-1)

is_prime :: (Num a, Integral a) => a -> Bool
is_prime n = is_prime_rec n (n-1)

primes2 = filter is_prime [2..]



---------------------------------

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