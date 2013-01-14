data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)
                          
