
import qualified Zora.Math as ZM
import qualified Data.Numbers.Primes as P
import qualified Data.MemoCombinators as Memo

ub :: Integer
ub = 40000000

chain_length :: Integer -> Integer
chain_length = Memo.integral chain_length'

chain_length' :: Integer -> Integer
chain_length' 1 = 1
chain_length' n =
    if P.isPrime n
        then 1 + chain_length (n - 1) -- phi(p) = p - 1
        else 1 + chain_length (ZM.euler_phi n)

result :: Integer
result
    = sum
    . filter ((==) 24 . chain_length . pred)
    . filter (> 9548416)
    . takeWhile (<= ub)
    $ P.primes

main :: IO ()
main = do
    putStrLn "See Python solution instead"
    print ub
    print result