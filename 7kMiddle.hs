module Codewars.Kata.Middle where

import           Data.List

-- Return the index of the middle element.
-- The first element has index 0.
-- gimme([2, 3, 1]) => 0
-- gimme([5, 10, 14]) => 1

gimme :: Ord a => (a, a, a) -> Int
gimme (a, b, c) = snd $ sort [(a1, 0), (b1, 1), (c1, 2)] !! 1 where
  a1 = cel1 (a, b, c)
  b1 = cel2 (a, b, c)
  c1 = cel3 (a, b, c)

cel1 (x,_,_) = x
cel2 (_,x,_) = x
cel3 (_,_,x) = x

-- best
gimme2 (a, b, c) = snd . (!! 1) . sort $ zip [a,b,c] [0..]
