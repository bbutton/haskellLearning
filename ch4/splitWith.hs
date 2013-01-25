-- splitWith :: (a -> Bool) -> [a] -> [[a]]
-- splitWith f []       = []
-- splitWith f (x:xs) | f x       = splitWith f xs -- consume all matching chars
--                    | otherwise = let (left, rest) = break f (x:xs) -- split as soon as a non-matching char is found
--                                  in left : splitWith f rest
                                     








splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f (x:xs)
    | f x = splitWith f xs
    | otherwise = let (prev, rest) = break f (x:xs)
                  in prev : splitWith f rest
      
          

