import qualified Data.List as List
import qualified Data.Map as Map
import List

factors :: Int -> [Int]
factors n
        | n == 1 = [1]
        | smallestFactor == n = [n]
        | otherwise = [smallestFactor] ++ (factors (n `div` (smallestFactor)))
        where smallestFactor = head (filter (\x -> mod n x == 0) [2..n])

powerpartition_rec :: [a] -> ([a], [a]) -> [([a], [a])]
powerpartition_rec src so_far
    | (length src == 0) = [so_far]
    | otherwise = l ++ r
    where l = powerpartition_rec (tail src) (((head src) : fst so_far), snd so_far)
          r = powerpartition_rec (tail src) (fst so_far, ((head src) : snd so_far))

powerpartition :: [a] -> [([a], [a])]
powerpartition l = powerpartition_rec l ([], [])

split_at_n n ps i =
  let (a, b) = splitAt i ps
      bigN = (sum a) + if b == [] then 0 else (product b)
      m = (length a) + if b == [] then 0 else 1 in
        (n - bigN) + m

n_to_ks n =
  let ps = factors n in
    map (split_at_n n ps) [1..((length ps)-1)]

look_wrapped ks n sum l =
  let kvals = n_to_ks n
      count = length [k | k <- kvals, k `elem` ks]
      newsum = sum + n in
        if count == (length ks)
          then l ++ [n]
          else look_wrapped (ks List.\\ kvals) (n + 1) newsum $ if count == 0 then l else l ++ [n]

look l = look_wrapped l 1 0 []

f x = look [2..x]

main = do
    putStrLn $ show $ f 12000