module Codewars.G964.Countdig where

import           Data.Char
-- n = 10, d = 1, the k*k are 0, 1, 4, 9, 16, 25, 36, 49, 64, 81, 100
-- We are using the digit 1 in 1, 16, 81, 100. The total count is then 4.
-- -- nb_dig(25, 1):
-- the numbers of interest are
-- 1, 4, 9, 10, 11, 12, 13, 14, 19, 21 which squared are 1, 16, 81, 100, 121, 144, 169, 196, 361, 441
-- so there are 11 digits `1` for the squares of numbers between 0 and 25.

-- Note that 121 has twice the digit 1.

nbDig :: Int -> Int -> Int
nbDig n d = length $ filter (== intToDigit d) $ concatMap (show . (^(2::Int))) [0..n]
