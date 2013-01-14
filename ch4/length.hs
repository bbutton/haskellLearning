length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

null' :: [a] -> Bool
null' [] = True
null' _ = False

head' :: [a] -> a
head' [] = error "can't get head from empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

last' :: [a] -> a
last' [] = error "can't get last from mepty list"
last' (x:[]) = x
last' (x:xs) = last' xs

init' :: [a] -> [a]
init' [] = error "can't init from empty list"
init' (x:[]) = []
init' (x:xs) = x: (init xs)

(+++) :: [a] -> [a] -> [a]
(+++) xs [] = xs
(+++) (x:[]) ys = x:ys
(+++) (x:xs) ys = x:(xs +++ ys)

concat' :: [[a]] -> [a]
concat' (x:[]) = x
concat' (x:xs) = x ++ (concat' xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:[]) = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || (or' xs)



