module Kata where

-- 1,9 -> 1,2,3,4,6,7,8,9 -> Result 8
-- 4,17 -> 4,6,7,8,9,10,11,12,13,14,16,17 -> Result 12

dontGiveMeFive :: Int -> Int -> Int
dontGiveMeFive start end = length $ filter (notElem '5' . show) [start..end]
