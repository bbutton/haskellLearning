-- Using Lemma 3 from Copmutation Geometry - Polygon Triangulation at 
-- http://valis.cs.uiuc.edu/~sariel/teach/courses/473/notes/19_triang.pdf

data Point = Point {
      x :: Int,
      y :: Int
    }
             deriving (Show)

data Angle = RightTurn | LeftTurn | Straight | Undefined deriving (Show)

isLeftTurn :: Point -> Point -> Point -> Angle
isLeftTurn p1 p2 p3
           | det < 0 = LeftTurn
           | det == 0 = Straight
           | det > 0 = RightTurn
    where p1MinusP2 = Point ((x p1) - (x p2)) ((y p1) - (y p2))
          p3MinusP2 = Point ((x p3) - (x p2)) ((y p3) - (y p2))
          det = ((x p1MinusP2) * (y p3MinusP2)) - ((x p3MinusP2) * (y p1MinusP2))
             
turnDirection :: [Point] -> [Angle]
turnDirection [] = []
turnDirection xs = calcDirection xs []
    where calcDirection [] ds = ds
          calcDirection xs ds
              | (length xs) < 3 = calcDirection [] ds
              | otherwise = calcDirection (tail xs) ((isLeftTurn (xs !! 0) (xs !! 1) (xs !! 2)):ds)

turnDirection' (a:b:c:xs) = (isLeftTurn a b c) : turnDirection' (b:c:xs)
turnDirection' _ = []
