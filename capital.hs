capital :: String -> String
capital "" = "Emtpy!!"
capital all@(x:xs) = "First letter of " ++ all  ++ " is " ++ [x]

