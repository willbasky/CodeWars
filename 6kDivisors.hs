module Divisors where

-- divisors 12   -- should return Right [2,3,4,6]
-- divisors 13   -- should return Left "13 is prime"
-- divisors 25   -- should return Right [5]

divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a | null n    = Left (show a ++ " is prime")
           | otherwise = Right n where
             n = modulator a

modulator :: Integral a => a -> [a]
modulator b = filter (/=0) $ map ((\x y -> if mod x y == 0 then y else 0) b) [2..b-1]

-- better way
divs a = filter ((==0) . mod a) [2..(a-1)]
