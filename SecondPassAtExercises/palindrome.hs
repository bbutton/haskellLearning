palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = [x] ++ (palindrome xs) ++ [x]

isPalindrome :: Eq t => [t] -> Bool
isPalindrome [] = False
isPalindrome xs = xs == (reverse xs)
                  
