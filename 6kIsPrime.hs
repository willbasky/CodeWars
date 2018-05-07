module IsPrime where

-- import qualified Data.Numbers.Primes as P

-- isPrime :: Integer -> Bool
-- isPrime = P.isPrime

isPrime :: Integer -> Bool
isPrime x | x < 2 = False
          | otherwise = not $ any divisible $ takeWhile notTooBig [2..] where
  divisible y = x `mod` y == 0
  notTooBig y = y*y <= x

-- First, we need a list of all elements y with y*y <= x:

-- takeWhile (\y ->  y*y <= x) [2..]

-- Then we need only the elements that divide x:

-- filter (\y ->  x `mod`y == 0) (takeWhile (\y ->  y*y <= x) [2..])

-- Then we need to check if that list is empty:

-- isPrime x = null (filter (\y ->  x `mod`y == 0) (takeWhile (\y ->  y*y <= x) [2..]))

-- And if this looks to lispy to you, replace some of the parens with $

-- isPrime x = null $ filter (\y ->  x `mod` y == 0) $ takeWhile (\y ->  y*y <= x) [2..]

-- For additional clarity you can "outsource" the lambdas:

-- isPrime x = null $ filter divisible $ takeWhile notTooBig [2..] where
--      divisible y = x `mod`y == 0
--      notTooBig y = y*y <= x

-- best
isPrime2 :: Integer -> Bool
isPrime2 x = go (abs x)

go :: Integer -> Bool
go 0 = False
go 1 = False
go n = null [i | i <- [2.. floor $ sqrt (fromInteger n)], n `mod` i == 0]
