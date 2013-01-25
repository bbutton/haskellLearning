-- process :: String -> String
-- process [] = []
-- process letters
--     | length wordList == 1 = letters ++ "\n"
--     | otherwise
--       let initialTransposition = transpose'
--     where wordList = words letters

--    unlines( transpose2 (words wordList))

transpose' :: String -> String -> [String]
transpose' [] _ = []
transpose' _ [] = []
transpose' (x:xs) (y:ys) = [x,y]:transpose' xs ys

transpose2 :: [String] -> String -> [String]
transpose2 [] _ = []
transpose2 _ [] = []
transpose2 (word:wordList) (letter:letters) = (word ++ [letter]): transpose2 wordList letters
