module DescendingOrder where

import Data.List

descendingOrder :: Integer -> Integer
descendingOrder = (\x -> read x::Integer) . sortBy (flip compare) . show


-- Если тпы указаны, то read и show разберутся
descendingOrder2 :: Integer -> Integer
descendingOrder2 = read . sortBy (flip compare) . show
