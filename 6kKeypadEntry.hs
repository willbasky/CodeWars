module Haskell.Codewars.KeypadEntry where

import           Data.Char
import           Data.List
import           Data.Maybe

-- "WHERE DO U WANT 2 MEET L8R" a teen would have to actually do 47 button presses.

presses :: String -> Int
-- presses str = fromMaybe 0 $ (<*>) (return sum) $ mapM ((\x -> fmap (+ 1) . elemIndex x . concat $ filter (elem x) phone ) . toLower) str

-- or
presses = sum . map ((\x -> (+ 1) . fromJust . elemIndex x . concat $ filter (elem x) phone ) . toLower)

phone :: [String]
phone = ["1", "abc2", "def3", "ghi4", "jkl5", "mno6", "pqrs7", "tuv8", "wxyz9", "*", " 0", "#"]
