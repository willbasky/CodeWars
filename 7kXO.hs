module Codewars.Kata.XO where

import Data.Char
-- XO("ooxx") => true
-- XO("xooxx") => false
-- XO("ooxXm") => true
-- XO("zpzpzpp") => true // when no 'x' and 'o' is present should return true
-- XO("zzoo") => false

-- | Returns true if the number of
-- Xs is equal to the number of Os
-- (case-insensitive)
-- xo :: String -> (Int, Int)
xo :: String -> Bool
xo = uncurry (==) . foldr ((\x (accX, accO) -> (if x =='x' then 1 + accX else accX, if x=='o' then 1 + accO else accO)) . toLower) (0,0)
