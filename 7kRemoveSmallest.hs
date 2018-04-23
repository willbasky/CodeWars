module Codewars.Kata.RemoveSmallest where

import Data.List
-- removeSmallest [1,2,3,4,5] = [2,3,4,5]
-- removeSmallest [5,3,2,1,4] = [5,3,2,4]
-- removeSmallest [2,2,1,2,1] = [2,2,2,1]

removeSmallest :: [Int] -> [Int]
removeSmallest xs = delete (minimum xs) xs
