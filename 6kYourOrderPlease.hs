module Codewars.Kata.YourOrderPlease where

import Data.Char
import Data.List
import Data.Ord
-- your_order("is2 Thi1s T4est 3a")
-- [1] "Thi1s is2 3a T4est"

yourOrderPlease :: String -> String
yourOrderPlease = unwords . map fst . sortBy (comparing snd) . map (\x -> (x, read (filter isDigit x)::Int)) . words

-- best
yourOrderPlease2 = unwords . sortBy (comparing (head . filter isDigit)) . words

-- sortOn is function of Data.List
yourOrderPlease3 = unwords . sortOn (find isDigit) . words
