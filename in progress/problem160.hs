
trim_0s_str :: String -> String
trim_0s_str s = case head s of
	'0' -> trim_0s_str $ tail s
	_ -> s

trim_to_len :: Integer -> Integer -> Integer
trim_to_len len x = x `mod` 10^len

trim_0s :: Integer -> Integer
trim_0s x = case x `mod` 10 of
	0 -> trim_0s $ div x 10
	_ -> x

mult_without_trailing_0s :: Integer -> Integer -> Integer
mult_without_trailing_0s x y = trim_to_len 5 $ trim_0s (x*y)

sought :: Integer
sought = foldl1 mult_without_trailing_0s [1..48828125]

main = do
	(putStrLn . show) sought