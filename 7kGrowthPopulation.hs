module Codewars.G964.Arge where

-- nb_year(1500, 5, 100, 5000) -> 15
-- nb_year(1500000, 2.5, 10000, 2000000) -> 10

nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p = untilCount (0::Int) (>=p) (oneYear percent aug) p0
 where
  untilCount years bool count men | bool men  =  years
                                  | otherwise =  untilCount (years + 1) bool (oneYear percent aug) (oneYear percent aug men)


oneYear :: Double -> Int -> Int -> Int
oneYear percent aug p0 = floor $ (fromIntegral p0 * (percent / 100)) + fromIntegral aug + fromIntegral p0

-- провести рекусрию 10 раз и остановится.
sum10 x = x + sum10 (2*x)
-- решение
sumNRec x 0     = 0
sumNRec x times = x + sumNRec (x*2) (times - 1)

sumTen x = sumNRec x 10

-- или так
sumTen2 :: Float -> Float
sumTen2 = go 10
    where
      go n x
       | n == 0 = x
       | otherwise = x + go (n-1) (2*x)

-- смена порядка трех аргументов
-- (flip.) . flip :: (a1 -> a2 -> b -> c) -> a2 -> b -> a1 -> c

-- until p f  yields the result of applying f until p holds.

until3            :: (a -> Bool) -> (a -> a) -> a -> a
until3 p f x
     | p x       =  x
     | otherwise =  until3 p f (f x)

-- best
nbYear2 p0 percent aug p
  = length
  $ takeWhile (< p)
  $ iterate ((+ aug ) . floor . (* (1+percent/100)) . fromIntegral) p0

nbYear3 p0 percent aug p
    | p0 >= p = 0
    | otherwise = 1 + nbYear3 (aug + floor (fromIntegral (p0 * 100) + fromIntegral p0 * percent) `div` 100) percent aug p
