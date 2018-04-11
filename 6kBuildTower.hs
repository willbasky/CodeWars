module Codewars.BuildTower where

-- import           Data.Bools
-- import           Data.List.Split

-- 6 floor
-- [
--   '     *     ',
--   '    ***    ',
--   '   *****   ',
--   '  *******  ',
--   ' ********* ',
--   '***********'
-- ]
buildTower :: Int -> [String]
buildTower n = map ((\x -> replicate ((big n - length x) `div` 2) ' ' ++ x ++ replicate ((big n - length x) `div` 2) ' ') . (`replicate` '*')) . numbers $ n
    where
        numbers z = take z [1,3..]
        big y = last (numbers y)

-- best
buildTower2 :: Int -> [String]
buildTower2 = foldl (\acc x -> map ((' ':) . (++" ")) acc ++ [replicate x '*']) [] . flip take [1,3..]
