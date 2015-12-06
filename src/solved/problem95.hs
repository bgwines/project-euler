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

sum_divisors :: Integer -> Integer
sum_divisors = Memo.integral sum_divisors'
	where sum_divisors' = sum . init . uniqueify . map product . powerset . factorize

calc_chain :: Integer -> [Integer]
calc_chain = Memo.integral calc_chain'
	where calc_chain' n =
		if n > 10000000 then
			[]
		else
			n : (calc_chain . sum_divisors $ n)

amicable_chains :: [[Integer]]
amicable_chains = filter is_amicable . map decyclify $ chains
	where
		chains = map calc_chain [1..1000000]
		is_amicable l = (head l) == sum_divisors (last l)

main = do
	(putStrLn . show . minimum . List.maximumBy (Ord.comparing length)) amicable_chains