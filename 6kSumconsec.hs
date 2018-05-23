module Codewars.G964.Sumconsec where

import           Data.List

sumConsecutives :: [Int] -> [Int]
sumConsecutives = map sum . group
