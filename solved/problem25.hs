
import Data.List

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

find_index l condition index =
	case condition $ head l of
		True  -> index
		False -> find_index (tail l) condition index+1

sought = find_index fibs (\x -> (length $ show x) == 1000) 1