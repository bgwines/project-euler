
import Data.List

bounciness b = init $ zipWith compare b (tail b ++ [last b])


custom_cmp LT LT = True
custom_cmp GT GT = True
custom_cmp EQ _  = True
custom_cmp _  EQ = True
custom_cmp _  _  = False

is_bouncy :: String -> Bool
is_bouncy l = length (groupBy custom_cmp $ bounciness l) > 1

integer_bounciness = map (is_bouncy . show) [1..]

f e
	| e = 1
	| otherwise = 0

integer_bounciness_binary = map f integer_bounciness

prefix_sums_with_indices = tail $ scanl
	(\(a,b) e -> (a+e, b+1)) (0,0)
	integer_bounciness_binary

sought = find (\(e,i) -> e/i >= 0.99) prefix_sums_with_indices