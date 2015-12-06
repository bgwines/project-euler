
import qualified Data.Char as Char

fact :: Integer -> Integer
fact n = product [1..n]

f :: Integer -> Bool
f n
	= (==) n
	. sum
	. map fact
	. map (toInteger . Char.digitToInt)
	. show
	$ n

main :: IO ()
main = do
	putStrLn . show $ sum . filter f $ [1..]