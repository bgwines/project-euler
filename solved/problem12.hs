
import qualified Data.List as List
import qualified Zora.Math as Math
import Data.Maybe

k :: Integer
k = 500

triangles :: [Integer]
triangles = scanl1 (+) [1..]

sought_pair :: Maybe Integer
sought_pair = List.find has_sufficient_num_factors triangles
	where
		has_sufficient_num_factors :: Integer -> Bool
		has_sufficient_num_factors n =
			(toInteger . length . Math.divisors $ n) > k

main :: IO ()
main = do
	putStrLn . show . fromJust $ sought_pair