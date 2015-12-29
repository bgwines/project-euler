
import qualified Zora.Math as ZM
import qualified Data.Numbers.Primes as P
import qualified Data.MemoCombinators as Memo

ub :: Integer
ub = 4000000--0

chain_length :: Integer -> Integer
chain_length = Memo.integral chain_length'

chain_length' :: Integer -> Integer
chain_length' 1 = 1
chain_length' n =
    if (ZM.prime_miller_rabin n) && (P.isPrime n)
        then 1 + chain_length (n - 1) -- phi(p) = p - 1
        else 1 + chain_length (ZM.euler_phi n)

result :: Integer
result = sum . filter ((==) 25 . chain_length) . takeWhile (<= ub) $ P.primes

main :: IO ()
main = do
    print ub
    print result