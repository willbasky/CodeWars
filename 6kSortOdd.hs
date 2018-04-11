module SortArray where

import Data.List
-- import Control.Lens
-- sortArray [5, 3, 2, 8, 1, 4] == [1, 3, 2, 8, 5, 4]

sortArray :: [Int] -> [Int]
sortArray = map snd . sortOn fst . (\x -> ((\(a,b) -> zip (sort a) (sort b)) . unzip . fst) x ++ snd x) . foldr (\y@(_,x) (accO,accE) -> if odd x then (y:accO, accE) else (accO, y:accE)) ([],[]) . zip [0..]

-- best
-- sortArray2 xs = xs & partsOf (each . filtered odd) %~ sort

sortArray3 = replaceOdd <$> id <*> sort . filter odd
  where replaceOdd xs [] = xs
        replaceOdd (x:xs) oos@(o:os)
          | even x    = x : replaceOdd xs oos
          | otherwise = o : replaceOdd xs os

replaceOdd :: Integral a => [a] -> [a] -> [a]
replaceOdd xs [] = xs
replaceOdd (x:xs) oos@(o:os)
        | even x    = x : replaceOdd xs oos
        | otherwise = o : replaceOdd xs os
