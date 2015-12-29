
import qualified Data.List as L (find)
import qualified Data.Numbers.Primes as P (primes)
import qualified Data.MemoCombinators as Memo

numFormations :: Integer -> Integer -> Integer
numFormations = Memo.memo2 Memo.integral Memo.integral numFormations'

numFormations' :: Integer -> Integer -> Integer
numFormations' 0 _ = 1
numFormations' n maxPrime
    = sum
    . map (\p -> numFormations (n - p) (min p (n - p)))
    . takeWhile (<= min maxPrime n)
    $ P.primes

main :: IO ()
main = do
    print $ L.find (\n -> numFormations n n > 5000) [1..]