module Codewars.Kata.MinMax where

-- minMax [1,2,3,4,5] `shouldBe` (1, 5)
-- minMax [2334454,5] `shouldBe` (5, 2334454)
-- minMax [1]         `shouldBe` (1, 1)

minMax :: (Ord a) => [a] -> (a, a)
minMax xs = (minimum xs, maximum xs)

-- best
-- minMax2 = liftA2 (,) minimum maximum
