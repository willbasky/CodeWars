module Codewars.G964.Getmiddle where

-- "test" should return "es"
-- "testing" should return "t"
-- "middle" should return "dd"
-- "A" should return "A"
getMiddle :: String -> String
getMiddle l@(_:_:_:_) = getMiddle $ tail $ init l
getMiddle l           = l
