inter :: a -> [[a]] -> [a]
inter sep [] = []
inter sep (x:xs) =  x ++ [sep] ++ inter sep xs
