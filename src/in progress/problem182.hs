
import Zora.Math (coprime)
import Data.List (groupBy, sortBy)

import Data.Ord

type E = Integer
type M = Integer
type C = Integer

{-
m^e = m (mod n)
m^e - m = kn for some k
k <= ((n-1)^(phi-1) - (n-1))/n
k <= something very very large

n, e fixed. e.g.
    m^4 - m = 703k
-}

encrypt :: E -> M -> C
encrypt e m = (m^e) `mod` n

p :: Integer
p = 19 --1009

q :: Integer
q = 37 --3643

n :: Integer
n = p * q

phi :: Integer
phi = (p - 1) * (q - 1)

-- 1 < e < phi, gcd(e, phi) = 1
es :: [E]
es = filter (coprime phi) [(1 + 1) .. (phi - 1)]

ms :: [M]
ms = [0 .. (n - 1)]

numIdentityEncryptions :: E -> Int
numIdentityEncryptions e
    = length
    . filter (\m -> m == encrypt e m)
    $ ms

bestEs :: [E]
bestEs
    = map fst
    . head
    . groupBy (\a b -> snd a == snd b)
    . sortBy (comparing snd)
    . map (\e -> (e, numIdentityEncryptions e))
    $ es

main :: IO ()
main = do
    print $ sum bestEs
