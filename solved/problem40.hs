
import qualified Data.Char as Char

s :: String
s = concatMap show [1..1000000]

index :: String -> Int -> Char
index s i = s !! (i - 1)

prod :: Int
prod
	= product
	. map (Char.digitToInt . index s)
	$ [1,10,100,1000,10000,100000,1000000]

main :: IO ()
main = do
	putStrLn . show $ prod