{-
If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
-}

all_combos ub =
	[(a,b,ub-a-b) |
		a <- [0..(ub-1)], b <- [a..ub-a], b <= ub-a-b,
		a*a + b*b == (ub-a-b)^2]

alls1 = [all_combos ub | ub <- [0..1000]]
alls = filter (\x -> length x /= 0) alls1

better a b
	| length a < length b = b
	| otherwise = a

m = foldl1 better alls