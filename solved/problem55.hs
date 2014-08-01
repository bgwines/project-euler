
import Data.Char

import Control.Applicative

powers :: [Integer]
powers = (^) <$> [1..99] <*> [1..99]

sought :: Int
sought
	= maximum
	. map (sum . map digitToInt . show)
	$ powers

main :: IO ()
main = do
	putStrLn . show $ sought