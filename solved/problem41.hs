
import Data.List
import Data.Char

import Prime
import Perms

----------------

pp9s = filter is_prime $ map read $ gen_perms "123456789" :: [Int]
pp8s = filter is_prime $ map read $ gen_perms "12345678" :: [Int]
pp7s = filter is_prime $ map read $ gen_perms "7654321" :: [Int]
pp6s = filter is_prime $ map read $ gen_perms "123456" :: [Int]
pp5s = filter is_prime $ map read $ gen_perms "12345" :: [Int]
pp4s = filter is_prime $ map read $ gen_perms "1234" :: [Int]
pp3s = filter is_prime $ map read $ gen_perms "123" :: [Int]
pp2s = filter is_prime $ map read $ gen_perms "12" :: [Int]