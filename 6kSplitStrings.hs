module Codewars.Kata.SplitStrings where

import           Data.List.Split
-- solution "abc" `shouldBe` ["ab", "c_"]
-- solution "abcdef" `shouldBe` ["ab", "cd", "ef"]

solution :: String -> [String]
solution xs | odd (length xs) = chunksOf 2 (init xs) ++ [last xs : "_"]
            | otherwise       = chunksOf 2 xs

-- best, i start to do like this and rest unfinished when icame across chunksOf
solution2 :: String -> [String]
solution2 []       = []
solution2 [x]      = [[x,'_']]
solution2 (x:y:xs) = [x,y]:solution2 xs
