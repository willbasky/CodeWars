module Codewars.Kata.Spinning where

import Data.List.Split

-- spinWords( "This is another test" )=> returns "This is rehtona test"
spinWords :: String -> String
spinWords str = error "todo: spinWords"

reverseWords xs = concatMap (\x -> if length x >= 5 then reverse x else x) (split (oneOf " ") xs)
