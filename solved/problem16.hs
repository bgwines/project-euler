
import Data.Char

digit_sum :: Int
digit_sum = sum $ map digitToInt $ show (2^1000)

main :: IO ()
main = do
	putStrLn . show $ digit_sum