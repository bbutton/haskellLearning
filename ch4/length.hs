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

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs)
    | f x == False = []
    | otherwise = x:takeWhile' f xs

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f xs
    | f (head xs) == True = dropWhile' f (tail xs)
    | otherwise = xs

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' f [] = ([],[])
span' f xs = ((takeWhile' f xs), (dropWhile' f xs))

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f [] = ([], [])
break' f xs = ((dropWhile' f xs), (takeWhile' f xs))

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a xs
      | null xs = False
      | (head xs) == a = True
      | otherwise = elem' a (tail xs)

notElem' :: (Eq a) => a -> [a] -> Bool
notElem' a [] = True
notElem' a xs
         | null xs = True
         | (head xs) == a = False
         | otherwise = notElem' a (tail xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
            | f x = x:filter' f xs
            | otherwise = filter' f xs

isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' ps xs = ps == ((take (length ps)) xs)


isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' [] _ = True
isInfixOf' _ [] = False
isInfixOf' ps xs
           | ps == ((take (length ps)) xs) = True
           | otherwise = isInfixOf' ps (tail xs)

isSuffixOf' :: (Eq a) => [a] -> [a]-> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' ss xs = ss == (drop startingChars xs)
                    where startingChars = (length xs) - (length ss)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y): zipWith' f xs ys
