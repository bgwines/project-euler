
{-
	T_0 = 2
	T_1 = 3
	T_2 = (8, 3)
	T_3 = (11, 4)
	
	let
		a'  = a_(T_(n-1))
		b'  = b_(T_(n-1))
		a'' = a_(T_(n-2))
		b'' = b_(T_(n-2))
	in

			  a' + (a'' / k)		ka' + a''
		T_n = -------------		=	---------
			  b' + (b'' / k)		kb' + b''
-}

import qualified Data.Char as Char

coeff_seq :: [Integer]
coeff_seq = concat $ map (\n -> [1, 2*n, 1]) [1..]

calc_term :: Integer -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
calc_term k (a', b') (a'', b'') = (a, b)
	where
		a = (k * a') + a''
		b = (k * b') + b''

-- fs    is the functions with 0 args applied
-- fs'   is the functions with the 1st arg applied
-- fs''  is the functions with the 2st arg applied, too
-- fs''' is the functions with the 3st arg applied, too (i.e. fully evaluated functions)
terms :: [(Integer, Integer)]
terms = (2, 1) : (3, 1) : (drop 1 fs''') -- drop 1 because of offset in fs''' (see below)
	where
		fs    = repeat calc_term
		fs'   = zipWith ($) fs   coeff_seq
		fs''  = zipWith ($) fs'  terms
		fs''' = zipWith ($) fs'' ((0, 0) : terms) -- prepend to offset to get (a'', b'')

sum_digits :: Integer -> Integer
sum_digits = toInteger . sum . map Char.digitToInt . show

main = do
	(putStrLn . show . sum_digits . fst) $ terms' !! 100
		where terms' = (1, 1) : terms