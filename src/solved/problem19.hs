
increment :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
increment (year, month, day) = (year', month', day')
	where
		day' :: Integer
		day' = day + (num_days_in_month year month)

		month' :: Integer
		month' = (month `mod` 12) + 1

		year' :: Integer
		year' = if month' == 1
			then year + 1
			else year

is_leap_year :: Integer -> Bool
is_leap_year year =
	((year `mod` 4) == 0) &&
	(((year `mod` 100) /= 0) || ((year `mod` 400) == 0))

num_days_in_month :: Integer -> Integer -> Integer
num_days_in_month year month =
	case month of
		1 -> 31
		2 -> if is_leap_year year
			then 29
			else 28
		3 -> 31
		4 -> 30
		5 -> 31
		6 -> 30
		7 -> 31
		8 -> 31
		9 -> 30
		10 -> 31
		11 -> 30
		12 -> 31

is_sunday :: Integer -> Bool
is_sunday day = (day `mod` 7) == 0

num_sundays :: Int
num_sundays
	= (+) 1
	. sum
	. map (\(y,m,d) -> if is_sunday d then 1 else 0)
	. takeWhile (\(y,m,d) -> y /= 2000)
	. iterate increment
	$ (1901, 1, 2)

main :: IO ()
main = do
	putStrLn . show $ num_sundays

