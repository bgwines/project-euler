module Prime
( primes
, is_prime
) where

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)

primes, nonprimes :: [Integer]
primes    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes) 
nonprimes = foldr1 f $ map g $ tail primes
  where 
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt
    
is_prime_rec :: Integer -> Integer -> Bool
is_prime_rec n k
	| (n <= 1) = False
	| (fromInteger k >= ((fromInteger n) / 2) + 1.0) = True
	| ((n `mod` k) == 0) = False
	| otherwise = is_prime_rec n (k+1)

is_prime :: Integer -> Bool
is_prime n = is_prime_rec n 2
