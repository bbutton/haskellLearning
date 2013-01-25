process :: String -> String
process [] = []
process xs = getAllFirstWords (lines xs)

getAllFirstWords :: [String] -> String
getAllFirstWords [] = []
getAllFirstWords (x:xs) =
    let firstWord = head $ words x
    in firstWord ++ "\n" ++ (getAllFirstWords xs)
