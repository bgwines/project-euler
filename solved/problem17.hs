
number_to_string :: String -> String
number_to_string n =
	case n of
		"1"  -> "one"
		"2"  -> "two"
		"3"  -> "three"
		"4"  -> "four"
		"5"  -> "five"
		"6"  -> "six"
		"7"  -> "seven"
		"8"  -> "eight"
		"9"  -> "nine"
		"10" -> "ten"
		"11" -> "eleven"
		"12" -> "twelve"
		"13" -> "thirteen"
		"14" -> "fourteen"
		"15" -> "fifteen"
		"16" -> "sixteen"
		"17" -> "seventeen"
		"18" -> "eighteen"
		"19" -> "nineteen"
		"20" -> "twenty"
		"30" -> "thirty"
		"40" -> "forty"
		"50" -> "fifty"
		"60" -> "sixty"
		"70" -> "seventy"
		"80" -> "eighty"
		"90" -> "ninety"

number_in_english :: String -> String
number_in_english n =
	case (length n) of
		1 -> number_to_string n
		2 -> if ((head n) == '1') || ((last n) == '0')
			then number_to_string n
			else unit_str ++ tens_str
			where
				unit_str :: String
				unit_str = number_to_string [n !! 1]

				tens_str :: String
				tens_str = number_to_string $ (n !! 0) : "0"
		3 -> if (tail n) == "00"
			then hundreds_str
			else hundreds_str ++ "and" ++ remaining
			where
				hundreds_str :: String
				hundreds_str = (number_to_string [n !! 0]) ++ "hundred"

				remaining :: String
				remaining = if (n !! 1) == '0'
					then number_in_english [n !! 2]
					else number_in_english $ tail n
		4 -> "onethousand"

get_letter_count :: Int -> Int
get_letter_count n = length . number_in_english . show $ n

main :: IO ()
main = do
	putStrLn . show $ sum . map get_letter_count $ [1..1000]
