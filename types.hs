type ISBN = Int
type Authors = [String]
type Title = String

data Review = BookReview ISBN Title Authors | MagazineReview ISBN Title Authors deriving Show
