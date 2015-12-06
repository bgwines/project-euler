import qualified Data.Char as Char
import qualified Data.List as List
import qualified Zora.List as ZList

prep :: [Integer] -> String
prep = map (Char.intToDigit . fromIntegral)

has_property :: String -> Bool
has_property s = and
	[ ((read . ZList.subseq 1 3 $ s) `mod` 2) == 0
	, ((read . ZList.subseq 2 3 $ s) `mod` 3) == 0
	, ((read . ZList.subseq 3 3 $ s) `mod` 5) == 0
	, ((read . ZList.subseq 4 3 $ s) `mod` 7) == 0
	, ((read . ZList.subseq 5 3 $ s) `mod` 11) == 0
	, ((read . ZList.subseq 6 3 $ s) `mod` 13) == 0
	, ((read . ZList.subseq 7 3 $ s) `mod` 17) == 0 ]

pandigitals :: [[Integer]]
pandigitals = ZList.permutations [0..9]

sought :: [[Integer]]
sought = filter (has_property . prep) pandigitals

ints :: [Integer]
ints = map read . map prep $ sought :: [Integer]