module Codewars.Kata.Square where

isSquare :: (Integral n) => n -> Bool
isSquare n | n < 0 = False
           | sqrt (fromIntegral n) == fromIntegral (round (sqrt (fromIntegral n))) = True
           | otherwise = False
