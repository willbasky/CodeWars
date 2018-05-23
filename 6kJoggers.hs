module Joggers where

-- nbrOfLaps 5 3 `shouldBe` (3, 5)
-- nbrOfLaps 4 6 `shouldBe` (3, 2)

nbrOfLaps :: Integer -> Integer -> (Integer, Integer)
nbrOfLaps bo ch = if ku bo ch == 1 then (ch, bo) else nbrOfLaps (bo `div` ku bo ch) (ch `div` ku bo ch)

ku :: Integral t => t -> t -> t
ku _ 0 = 0
ku a b = if mod a b == 0 then b else ku b (mod a b)

-- есть функция gcd - greatest common divisor и lcm - lowest common multiple
-- или с библиотекой
-- import Data.Ratio

-- nbrOfLaps bob charles = (denominator rat, numerator rat)
--   where
--     rat = bob % charles
