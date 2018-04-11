module Codewars.Kata.Compare where

import Data.List

comp :: [Integer] -> [Integer] -> Bool
comp as bs = sort (map (^2) as) == sort bs

a = [121, 144, 19, 161, 19, 144, 19, 11, 1]
b = [121, 14641, 20736, 361, 25921, 361, 20736, 361, 1]
