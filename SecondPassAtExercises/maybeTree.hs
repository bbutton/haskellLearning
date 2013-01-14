data MTree a = MTree (Maybe a) (Maybe (MTree a)) (Maybe (MTree a)) deriving (Show)

