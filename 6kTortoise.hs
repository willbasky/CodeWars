module Codewars.G964.Tortoise where

-- testTortoise 720 850 70 (Just (0, 32, 18))
-- testTortoise 80 100 40 (Just (2, 0, 0))
-- testTortoise 80 91 37 (Just (3, 21, 49))

race :: Int -> Int -> Int -> Maybe (Int, Int, Int)
race v1 v2 g  | v1 >= v2  = Nothing
              | otherwise = Just (h, m, s) where
                  h = floor x
                  m = mod (floor ((x * 60)::Double)) 60
                  s = mod (floor ((x * 3600)::Double)) 60
                  x = fromIntegral g / fromIntegral (v2 - v1)
