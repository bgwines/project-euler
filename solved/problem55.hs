
import Data.Char

powers = [[a^b | b <- [1..99]] | a <- [1..99]]

sought = maximum $ map sum $ map (map digitToInt) $ map show $ concat powers