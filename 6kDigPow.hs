module Codewars.Kata.DigPow where

-- digpow 92 1 should return -1 since there is no k such as 9¹ + 2² equals 92 * k
-- digpow 695 2 should return 2 since 6² + 9³ + 5⁴= 1390 = 695 * 2
-- (a ^ p + b ^ (p+1) + c ^(p+2) + d ^ (p+3) + ...) = n * k

digpow :: Integer -> Integer -> Integer
digpow n p | fromIntegral number / fromIntegral n == fromIntegral (number `div` n) = number `div` n
           | otherwise = -1
              where
                list = reverse . digsList $ n
                number = summator p list


summator :: Integer -> [Integer] -> Integer
summator _ [] = 0
summator p (x:xs) = x^p + summator (p+1) xs

digsList :: Integer -> [Integer]
digsList 0 = []
digsList x = x `mod` 10 : digsList (x `div` 10)

-- best. И тут есть проверка делится ли число на число без остатка.
digpow2 n p | sp `mod` n == 0  = sp `div` n
            | otherwise        = -1
  where sp = fromIntegral $ sum $ zipWith (^) (map digitToInt $ show n) [p..]
