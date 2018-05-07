module Codewars.Kata.Which where

import           Data.List

-- a1 = ["arp", "live", "strong"]
-- a2 = ["lively", "alive", "harp", "sharp", "armstrong"]
-- returns ["arp", "live", "strong"]
-- #Example 2: a1 = ["tarp", "mice", "bull"]
-- a2 = ["lively", "alive", "harp", "sharp", "armstrong"]

inArray :: [String] -> [String] -> [String]

inArray a1 a2 | null a1 || null a2 = []
              | otherwise          = sort $ nub $ filter (`isInfixOf` list) a1
  where
    list = unwords a2

-- best
inArray2 a1 a2 = sort $ nub [x | x <- a1, y <- a2, x `isInfixOf` y]
