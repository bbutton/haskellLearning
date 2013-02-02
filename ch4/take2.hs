getColumn :: [[a]] -> [a]
--getColumn ((r:[]):[]) = error "help"
getColumn [] = []
--getColumn ([]:[]) = []
getColumn ((r:row):[]) = r:(getColumn [])
getColumn ((r:row):rs) = r:(getColumn rs)

stripColumn :: [[a]] -> [[a]]
stripColumn [] = []
stripColumn (row:[]) = (tail row):stripColumn []
stripColumn (row:rs) = (tail row):stripColumn rs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose rows = (getColumn rows) : (transpose $ stripColumn rows)

-- transpose (row:[]) = getColumn (row:[]):transpose (tail row):[]
