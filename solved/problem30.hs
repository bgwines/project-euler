
import qualified Zora.List as ZList
import qualified Data.Char as Char

valid_ns :: [Integer]
valid_ns = filter (\n -> (n == (f n))) $ [1..]
	where
		f :: Integer -> Integer
		f = sum . map ((^) 5 . Char.digitToInt) . show

main :: IO ()
main = do
	putStrLn . show $ take 3 valid_ns