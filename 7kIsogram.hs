module Isogram where

import Data.List(nub)
import Data.Char(toLower)
-- isIsogram "Dermatoglyphics" == true
-- isIsogram "moose" == false
-- isIsogram "aba" == false

isIsogram :: String -> Bool
isIsogram str = length (nub strL) == length strL where
  strL = map toLower str
