module BreakingChocolate where

-- 5 5 ~> 24
breakChocolate :: Int -> Int -> Int
breakChocolate _ 0 = 0
breakChocolate 0 _ = 0
breakChocolate m n = (m*n)-1
