
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

               a     a' + (a'' / k)     ka' + a''
        T_n =  -  =  --------------  =  ---------
               b     b' + (b'' / k)     kb' + b''
-}

import qualified Data.Char as Char

coeff_seq :: [Integer]
coeff_seq = concat $ map (\n -> [1, 2*n, 1]) [1..]

calc_term :: Integer -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
calc_term k (a', b') (a'', b'') = (a, b)
	where
		a = (k * a') + a''
		b = (k * b') + b''

-- fs   : functions with 0 args applied
-- fks  : functions with the 1st arg applied
-- fs'  : functions with the 2st arg applied, too
-- fs'' : functions with the 3st arg applied, too
terms :: [(Integer, Integer)]
terms = (2, 1) : (3, 1) : (drop 1 fs'') -- drop 1 because of offset in fs''' (see below)
	where
		fs   = repeat calc_term
		fks  = zipWith ($) fs  coeff_seq        -- apply k_i to f_i
		fs'  = zipWith ($) fks terms            -- apply T_(i-1) = (a' , b' ) to f_i
		fs'' = zipWith ($) fs' ((0, 0) : terms) -- apply T_(i-2) = (a'', b'') to f_i
												-- (prepend (0, 0) to offset to get (a'', b''))

sum_digits :: Integer -> Integer
sum_digits = toInteger . sum . map Char.digitToInt . show

main = do
	(putStrLn . show . sum_digits . fst) $ terms' !! 100
		where terms' = (1, 1) : terms