module Codewars.G964.Sumdigpow where

-- import           Data.Digits
-- 135 = 1^1 + 3^2 + 5^3
-- sum_dig_pow(1, 10) == [1, 2, 3, 4, 5, 6, 7, 8, 9]
-- sum_dig_pow(1, 100) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 89]

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = [x | x <- [a..b], x == check x]

check :: Int -> Int
check x = sum $ zipWith (^) (reverse $ digs x) [1..]


digs :: Int -> [Int]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

-- Library didn`t import
-- check x = sum $ zipWith (^) (digits 10 x) [1..]
