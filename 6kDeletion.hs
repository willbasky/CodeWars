module Codewars.Kata.Deletion where

import           Data.List
-- deleteNth [20,37,20,21]       1 `shouldBe` [20,37,21]
-- deleteNth [1,1,3,3,7,2,2,2,2] 3 `shouldBe` [1, 1, 3, 3, 7, 2, 2, 2]

-- deleteNth :: [Int] -> Int -> [Int]
-- deleteNth = (\ x -> (length . filter (==2) $ x) > 2) [1, 1, 3, 3, 7, 2, 2, 2, 2]
-- deleteNth :: [[Integer]]
-- deleteNth = map (\x -> if (length . filter (==x) $ lst) > 2 then delete x lst else lst) $ nub lst
--     where
--         lst = [1, 1, 3, 3, 3, 7, 2, 2, 2, 2]

deleteNth = deleteBy (\x -> (length . filter (x == head lst) $ lst) > 2) (head lst) lst
    where
        lst = [1, 1, 3, 3, 3, 7, 2, 2, 2, 2]



