-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = process

process :: String -> String
process [] = []
process xs = getAllFirstWords (lines xs)

getAllFirstWords :: [String] -> String
getAllFirstWords [] = []
getAllFirstWords (x:xs) = (getFirstWord x) ++ "\n" ++ (getAllFirstWords xs)

getFirstWord :: String -> String
getFirstWord [] = []
getFirstWord  xs = fst (span isNotSpace xs)

isNotSpace :: Char -> Bool
isNotSpace x = x /= ' '
