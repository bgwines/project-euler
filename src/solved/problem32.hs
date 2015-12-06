
import qualified Data.List as List
import qualified Data.Char as Char

import qualified Zora.List as ZList

import Data.Maybe

pandigital :: String -> Bool
pandigital n = "123456789" == (List.sort n)

divisor_pairs :: Integer -> [(Integer, Integer)]
divisor_pairs n
	= map (ZList.map_snd (div n))
	. (\x -> zip x x)
	. filter (\m -> (n `mod` m) == 0) 
	$ [2..(div n 2)]

pandigital_trio :: (Integer, Integer) -> Bool
pandigital_trio mult_pair
	= pandigital $ 
		(show . fst $ mult_pair) ++
		(show . snd $ mult_pair) ++
		(show . ZList.pair_op (*) $ mult_pair)

sought :: [Integer]
sought
	= map (ZList.pair_op (*) . head)
	. filter (any pandigital_trio)
	. map divisor_pairs
	$ [1..1000000]

main :: IO ()
main = do
	putStrLn . show $ sought
