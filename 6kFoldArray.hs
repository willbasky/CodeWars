module Folded.MyLists where

-- Fold 1-times:
-- [1,2,3,4,5] -> [6,6,3]
-- Fold 2-times:
-- [1,2,3,4,5] -> [9,6]

foldList :: [Int] -> Int -> [Int]
foldList xs 0 = xs
foldList xs n = foldList (counter xs) (n - 1)

counter :: [Int] -> [Int]
counter [] = []
counter [x] = [x]
counter ys@(x:xs) = (x + last xs) : counter (init xs)
