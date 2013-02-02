getColumn :: [[a]] -> [a]
getColumn [] = []
getColumn ([]:rows) = []
getColumn ((r:row):[]) = r:(getColumn [])
getColumn ((r:row):rs) = r:(getColumn rs)

stripColumn :: [[a]] -> [[a]]
stripColumn [] = []
stripColumn ([]:rs) = []
stripColumn (row:[]) = (tail row):[]
stripColumn (row:rs) = (tail row):stripColumn rs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:rs) = []
transpose rows = (getColumn rows) : (transpose $ stripColumn rows)
