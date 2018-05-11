module Narcissistic where

-- import           Data.Char

-- 1^3 + 5^3 + 3^3 = 1 + 125 + 27 = 153
-- 1^4 + 6^4 + 3^4 + 4^4 = 1 + 1296 + 81 + 256 = 1634

narcissistic :: Integral n => n -> Bool
narcissistic n = sum (map (^length (go n)) (go n)) == n

go :: Integral b => b -> [b]
go = map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

-- checked :: Integral n => n -> n
-- checked x = fromIntegral . sum . map ((^(length $ show $ toInteger x)) . digitToInt) . show $ toInteger x

