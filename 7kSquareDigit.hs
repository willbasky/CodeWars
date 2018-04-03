module SquareDigit where
import Data.Char

squareDigit :: Int -> Int
squareDigit x | x >= 0 = expression x
              | otherwise = negate (expression (abs x))
                where
                  expression y = read (concatMap (show . (^(2::Int)) . digitToInt) (show y))::Int
