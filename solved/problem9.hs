{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

triplets = [(a,b,c) | a<-[1..998], b<-[(a+1)..999], c<-[(b+1)..1000], a+b+c==1000]

pythag_triplets = filter (\(a,b,c) -> a*a + b*b == c*c) triplets