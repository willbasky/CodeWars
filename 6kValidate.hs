module Validate where

-- 1714 ==> [1*, 7, 1*, 4] ==> [2, 7, 2, 4]
-- 12345 ==> [1, 2*, 3, 4*, 5] ==> [1, 4, 3, 8, 5]
-- 891 ==> [8, 9*, 1] ==> [8, 18, 1]
-- [8, 18*, 1] ==> [8, (1+8), 1] ==> [8, 9, 1]
-- [8, 18*, 1] ==> [8, (18-9), 1] ==> [8, 9, 1]
-- [8, 9, 1] ==> 8 + 9 + 1 = 18
-- 18 `mod` 10 ==> 8 ==> False else True

validate :: Integer -> Bool
validate n  | odd (length nList) = mod (sum (map29 $ filterEven nList) + sum (filterOdd nList)) 10 == 0
            | otherwise = mod (sum (map29 $ filterOdd nList) + sum (filterEven nList)) 10 == 0
                where
                  map29 = map ((\ x -> if x > 9 then x - 9 else x) . (*2))
                  nList = go n

go :: Integral b => b -> [b]
go = reverse . map (`mod` 10) . takeWhile (> 0) . iterate (`div` 10)

filterEven :: [Integer] -> [Integer]
filterEven []       = []
filterEven [_]      = []
filterEven (_:e:xs) = e : filterEven xs

filterOdd :: [Integer] -> [Integer]
filterOdd []       = []
filterOdd [o]      = [o]
filterOdd (o:_:xs) = o : filterOdd xs

-- best
-- validate = (== 0) . (`mod` 10) . sum . zipWith ($) (cycle [id, sum . digits . (*2)]) . reverse . digits
--   where digits = map (read . return) . show

digits :: Int -> [Int]
digits = map (read . return) . show
