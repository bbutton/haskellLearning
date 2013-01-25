-- process :: String -> String
-- process [] = []
-- process letters
--     | length wordList == 1 = letters ++ "\n"
--     | otherwise
--       let initialTransposition = transpose'
--     where wordList = words letters

--    unlines( transpose2 (words wordList))

prepare :: [a] -> (a, a, [a])
prepare (first:second:rest) = (first, second, rest)

transpose1 :: String -> String -> [String]
transpose1 [] _ = []
transpose1 _ [] = []
transpose1 (x:xs) (y:ys) = [x,y]:transpose1 xs ys

transpose2 :: [String] -> String -> [String]
transpose2 [] _ = []
transpose2 _ [] = []
transpose2 (word:wordList) (letter:letters) = (word ++ [letter]): transpose2 wordList letters
