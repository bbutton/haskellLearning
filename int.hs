inter' sep [] = []
inter' sep (x:[]) = x
inter' sep (x:xs) = x ++ [sep] ++ inter' sep xs
