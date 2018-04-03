module Codwars.Kata.Duplicates where

import Data.List(group,sort)
import Data.Char(toLower)

-- "Indivisibilities" -> 2 # 'i' occurs seven times and 's' occurs twice
duplicateCount :: String -> Int
duplicateCount str = full str - nodub str  where
  full = length . lowGroup
  nodub = length . filter (\x -> length x == 1) . lowGroup
  lowGroup = group . sort . map toLower

-- best
-- duplicateCount = length . filter ((> 1) . length) . group . sort . map toLower

-- x = "sdfsdff"
