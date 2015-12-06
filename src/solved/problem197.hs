{- Given is the function f(x) = ⌊230.403243784-x2⌋ × 10-9 ( ⌊ ⌋ is the floor-function),
the sequence un is defined by u0 = -1 and un+1 = f(un).

Find un + un+1 for n = 1012.
Give your answer with 9 digits after the decimal point. -}

f :: Double -> Double
f x = numerator / denominator
	where
		numerator :: Double
		numerator = fromIntegral . floor $ 2.0**(30.403243784 - x^2)

		denominator :: Double
		denominator = 10^9

sought :: Double
sought
	= read
	. take 11
	. show
	. sum
	. drop (998)
	. take 1000
	. iterate f
	$ -1

main :: IO ()
main = do
	putStrLn . show $ sought