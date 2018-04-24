module Codewars.G964.Maxdifflength where

-- s1 = ["hoqq", "bbllkw", "oox", "ejjuyyy", "plmiis", "xxxzgpsssa", "xxwwkktt", "znnnnfqknaz", "qqquuhii", "dvvvwz"]
-- s2 = ["cccooommaaqqoxii", "gggqaffhhh", "tttoowwwmmww"]
-- mxdiflg(s1, s2) --> 13

mxdiflg :: [String] -> [String] -> Maybe Int
mxdiflg s1 s2 | null s1 || null s2 = Nothing
              | otherwise = Just $ max (abs (maximum d1 - minimum d2)) (abs (maximum d2 - minimum d1)) where
                d1 = map length s1
                d2 = map length s2

