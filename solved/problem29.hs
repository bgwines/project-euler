
import qualified Zora.List as ZList

import Control.Applicative

nums :: [Integer]
nums = [2..100]

unique_powers :: [Integer]
unique_powers = ZList.uniqueify $ (^) <$> nums <*> nums

main :: IO ()
main = do
	putStrLn . show $ length unique_powers