module FindShortest where

findShortest :: String -> Integer
findShortest = minimum . map (toInteger . length) . words
