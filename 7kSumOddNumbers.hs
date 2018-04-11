module Codewars.SumOddNumbers where

-- rowSumOddNumbers 1 -- 1
-- rowSumOddNumbers 2 -- 3 + 5 = 8
-- rowSumOddNumbers 3 -- 7 + 9 + 11 = 27
-- rowSumOddNumbers 4 -- 13 + 15 + 17 + 19 = 64

rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers = (^3)

rowSumOddNumbers2 n =
  let x = sum [1..(n - 1)] * 2
  in x * n + sum (take (fromIntegral n) [1, 3..])

-- мое первое решение, хаскельное.
rowSumOddNumbers3 n = sum $ listOfNum 1 [1,3..] !! fromIntegral (n-1)
    where
      listOfNum n x = take n x : listOfNum (n+1) (drop n x)
