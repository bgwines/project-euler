{-
The proper divisors of a number are all the divisors excluding the number itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of these divisors is equal to 28, we call it a perfect number.

Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.

Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:

12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)

Since this chain returns to its starting point, it is called an amicable chain.

Find the smallest member of the longest amicable chain with no element exceeding one million.
-}

import Euler
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Ord as Ord
import qualified Data.MemoCombinators as Memo

sentinel = -1
repeaters = [1, 6]

get_divisors :: Integer -> [Integer]
get_divisors = Memo.integral get_divisors'
	where get_divisors' = init . uniqueify . map product . powerset . factorize

{-cut_at_cycle_rec :: (Ord a) => Set.Set a -> [a] -> [a]
cut_at_cycle_rec s l = 
	case e `Set.member` s of
		True -> []
		False -> e : cut_at_cycle_rec (Set.insert e s) (tail l)
		where e = head l

cut_at_cycle :: (Ord a) => [a] -> [a]
cut_at_cycle l = cut_at_cycle_rec Set.empty l-}

calc_chain_rec :: Integer -> [Integer]
calc_chain_rec n
	| n > 1000000 = [sentinel]
	| n `elem` repeaters = [n, sentinel]
	| otherwise = n : (calc_chain . sum . get_divisors $ n)

--calc_chain_rec' = cut_at_cycle calc_chain_rec''

calc_chain :: Integer -> [Integer]
calc_chain = Memo.integral calc_chain'
	where calc_chain' n = n : (takeWhile ((/=) n) $ tail (calc_chain_rec n))

chains :: [[Integer]]
chains = map calc_chain [1..1000000] 

main = do
	(putStrLn . show . minimum . List.maximumBy (Ord.comparing length)) chains