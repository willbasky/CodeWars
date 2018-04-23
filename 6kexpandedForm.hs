module ExpandedForm where

import Data.List(intercalate)

-- expandedForm 70304 -- Should return '70000 + 300 + 4'
expandedForm :: Int -> String
-- expandedForm n = concat $ tail $ foldr (\x acc -> " + " : show x : acc) [] $ filter (/=0) $ map (\(x,y) -> x * (10^y)) (digsListRange n)
expandedForm n = intercalate " + " $ filter (/= "0") $ map (\(x,y) -> show (x * (10^y))) (digsListRange n)

digsList :: Int -> [Int]
digsList 0 = []
digsList x = x `mod` 10 : digsList (x `div` 10)

digsListRange :: Int -> [(Int, Int)]
digsListRange x = reverse $ zip (digsList x) [(0::Int)..]



-- range :: Int -> Int
-- range 0 = 0
-- range n = 1 + range (fst (divMod n 10))
