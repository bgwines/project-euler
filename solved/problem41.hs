
import qualified Data.List as List
import qualified Data.Char as Char

import qualified Zora.Math as ZMath
import qualified Zora.List as ZList

prime_permutations :: String -> [Integer]
pp s = filter ZMath.prime . map read . ZList.permutations $ s :: [Integer]

pp9s = pp "123456789"
pp8s = pp "12345678"
pp7s = pp "7654321"
pp6s = pp "123456"
pp5s = pp "12345"
pp4s = pp "1234"
pp3s = pp "123"
pp2s = pp "12"