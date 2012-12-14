rightTriangles :: (Enum t, Eq t, Num t) => t -> [(t, t, t)]
rightTriangles perimeter = [ (a,b,c) | c <- [1..perimeter], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == perimeter ]
