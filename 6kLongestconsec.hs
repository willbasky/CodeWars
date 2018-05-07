module Codewars.G964.Longestconsec where

import           Data.List
-- #Example: longest_consec(["zone", "abigail", "theta", "form", "libe", "zas", "theta", "abigail"], 2) --> "abigailtheta"

-- n being the length of the string array, if n = 0 or k > n or k <= 0 return "".

longestConsec :: [String] -> Int -> String
longestConsec strarr k | n == 0 || k > n || k < 0 = ""
                        | otherwise = maximumBy (\x y -> compare (length x) (length y)) $ reverse $ consec strarr k
                        where
                          n = length strarr

consec :: [String] -> Int -> [String]
consec [] _        = []
consec zs@(_:xs) k = concat (take k zs) : consec xs k

-- не проканало, так как позиция слова имеет значение, и вообще нужна самая последовательность из k слов без смены позиций
longestConsec2 strarr k | n == 0 || k > n || k < 0 = ""
                        | otherwise = concat $ take k $ reverse $ nub $ sortOn length strarr
                        where
                          n = length strarr
