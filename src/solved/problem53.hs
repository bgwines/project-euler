
import qualified Zora.List as ZList
import qualified Data.MemoCombinators as Memo

fact :: Integer -> Integer
fact n = product [1..n]

c :: Integer -> Integer -> Integer
c = Memo.memo2 Memo.integral Memo.integral c'
	where
		c' :: Integer -> Integer -> Integer
		c' n r = (fact n) `div` ((fact r) * (fact $ n - r))

total_num_valid_pairs :: Integer
total_num_valid_pairs = sum . map num_valid_pairs $ [1..100]
	where
		num_valid_pairs :: Integer -> Integer
		num_valid_pairs n = ZList.count (\r -> 1000000 < (c n r)) [1..n]

main :: IO ()
main = do
	putStrLn . show $ total_num_valid_pairs