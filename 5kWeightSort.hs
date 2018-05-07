module Codewars.G964.WeightSort where

import           Data.Char
import           Data.List

-- estWeightSort "103 123 4444 99 2000" "2000 103 123 4444 99"
-- testWeightSort "9999 10003 2000 44444444 9999" "2000 10003 44444444 9999 9999"
-- "56 65 74 100 99 68 86 180 90" ordered by numbers weights becomes: "100 180 90 56 65 74 68 86 99"
-- When two numbers have the same "weight", let us class them as if they were strings and not numbers: 100 is before 180 because its "weight" (1) is less than the one of 180 (9) and 180 is before 90 since, having the same "weight" (9) it comes before as a string.

orderWeight :: String -> String
orderWeight str = unwords $ map snd (sort $ zip (sumer strW) strW)
  where
    strW = words str

-- Функия которая суммирует числа одного числа или список чисел?
-- Функция которая сравнивает

sumer :: [String] -> [Int]
sumer = map (sum . map digitToInt)
