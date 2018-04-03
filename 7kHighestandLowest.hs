module Kata (highAndLow) where

-- highAndLow "1 2 3 4 5")  # return "5 1"
-- highAndLow "1 2 -3 4 5") # return "5 -3"
-- highAndLow "1 9 3 4 -5") # return "9 -5"

highAndLow :: String -> String
highAndLow input = unwords $ map show [maximum (filtred input), minimum (filtred input)]
  where
    filtred = map (\x -> read x::Int) . words
