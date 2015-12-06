
import qualified Zora.Math as ZMath

find_index :: [a] -> (a -> Bool) -> Int -> Int
find_index (x:xs) f i =
	if f x
		then i
		else find_index xs f (succ i)

satisfying_index :: Int
satisfying_index = find_index ZMath.fibs right_length 1
	where
		right_length :: (Show a) => a -> Bool
		right_length x = (length . show $ x) == 1000

main :: IO ()
main = do
	putStrLn . show $ satisfying_index