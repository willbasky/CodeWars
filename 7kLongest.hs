module Codewars.G964.Longest where

import Data.List

a = "xyaabbbccccdefww"
b = "xxxxyyyyabklmopq"
-- longest(a, b) -> "abcdefklmopqwxy"

longest :: String -> String -> String
longest s1 s2 = sort . nub $ union s1 s2
