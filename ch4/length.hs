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

all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (x:xs) = (f x) && (all' f xs)

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = (f x) || (any' f xs)

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 xs = []
take' n (x:xs) = x:take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([], [])
splitAt' n xs = (take' n xs, drop' n xs)





