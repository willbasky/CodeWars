module Divisors where

divisors :: Integral a => a -> Int
divisors x = length [y | y <- [1..x], x `mod` y == 0]
