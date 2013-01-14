palindrome [] = []
palindrome (x:xs) = [x] ++ palindrome xs ++ [x]


myPalindrome :: [a] -> [a]
myPalindrome xs = xs ++ myReverse xs []
    where myReverse [] n = n
          myReverse (x:xs) n = myReverse xs (x:n)

isPalindrome [] = True
isPalindrome xs = palindromic xs True
    where palindromic []  isPal = isPal
          palindromic (x:[]) isPal = palindromic [] isPal
          palindromic (x:xs) isPal
                      | isPal == False = palindromic (init xs) False
                      | x /= last(xs) = palindromic (init xs) False
                      | otherwise = palindromic (init xs) True

