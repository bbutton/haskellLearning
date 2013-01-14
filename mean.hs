sum' :: Num a => [a] -> a
sum' [] = 0
sum' xs = summer xs 0
          where summer (x:xs) sum = summer xs (sum + x)
                summer [] count = count
     
mean :: Fractional a => [a] -> a
mean [] = 0
mean xs = sum' xs / (fromIntegral $ length xs)
