-- let wordList = words letters
--     (first, second, rest) = prepare wordList
--     seedList = transpose1 first second
                              
prepare :: [a] -> (a, a, [a])
prepare (first:second:rest) = (first, second, rest)

transpose1 :: String -> String -> [String]
transpose1 [] _ = []
transpose1 _ [] = []
transpose1 (x:xs) (y:ys) = [x,y]:transpose1 xs ys

