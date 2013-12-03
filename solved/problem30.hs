
import Data.Char

f = sum . (map (^5)) . (map digitToInt) . show

ns_and_fns = map (\n -> (n, f n)) [1..]

valid_ns = map (\(n, fn) -> n) $ filter (\(n, fn) -> n == fn) ns_and_fns