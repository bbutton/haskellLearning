listLength :: (Num b) => [a] -> b
listLength [] = 0
listLength (x:xs) = 1 + (listLength xs)

listMean :: Fractional t => [t] -> t
listMean [] = 0;
listMean xs = (sum xs) / (fromIntegral $ length xs)
    where sum [] = 0
          sum (x:xs) = x + sum(xs)

          length [] = 0
          length (x:xs) = 1 + length(xs)

