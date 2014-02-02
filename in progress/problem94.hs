{-
It is easily proved that no equilateral triangle exists with integral length sides and integral area. However, the almost equilateral triangle 5-5-6 has an area of 12 square units.

We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.

Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).
-}

ub = 1000000000

is_int x = x == fromInteger (round x)

area p a b c = 
    sqrt $ p * (p-a) * (p-b) * (p-c)

has_int_area (a,b,c) =
    let p = (a+b+c)/2
    in  case is_int p of True -> is_int $ area p a b c
                         False -> False

triangles = [(a,b,c) | a<-[1..ub], b<-[a+1], c<-[a,b], a+b+c <= ub]

almost_equilateral = filter has_int_area triangles

perimeters = map (\(a,b,c) -> a+b+c) almost_equilateral

sought = sum perimeters

main = do
    putStrLn $ show sought