module MultiplesOf3And5 where

import           Data.List (nub)
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

solution :: Integer -> Integer
solution number = modulator (number-1) 0 where
  modulator 0 x = x
  modulator number x = (if mod number 3 == 0 || mod number 5 == 0 then number else 0) + modulator (number-1) x

--best
solution2 n = sum $ nub $ [3,6..n-1] ++ [5,10..n-1]

solution3 :: Integer -> Integer
solution3 number = sum [n | n <- [1..number - 1], n `mod` 3 == 0 || n `mod` 5 == 0]

solution4 n = sum $ filter (\x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)) [1..n-1]
