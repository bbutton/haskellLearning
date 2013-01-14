length' :: [a] -> Int

length' [] = 0
length' xs = counter xs 0
    where counter (x:xs) count = counter xs (count + 1)
          counter [] count = count

main = length' [0..100000000]
