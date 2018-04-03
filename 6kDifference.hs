module Difference where

import Data.List.Split

difference :: Eq a => [a] -> [a] -> [a]
difference a b = concat $ split (dropBlanks . dropDelims $ oneOf b) a

-- best
-- difference a b = filter (`notElem` b) a
