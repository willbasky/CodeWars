module Reverse where

import           Data.List (groupBy)
import           Data.Char (isSpace)

-- reverseWords "An example!"    -- "nA !elpmaxe"
-- reverseWords "double  spaces" -- "elbuod  secaps"
-- верное решение
reverseWords :: String -> String
reverseWords = concatMap reverse . groupBy (\x y -> x /= ' ' && y /= ' ')
-- "Hello World!\na" ~> "olleH a\n!dlroW"

-- мое решение не работает на \n, которое считает за пробел
reverseWords2 :: String -> String
reverseWords2 = concatMap reverse . groupBy (\x y -> isSpace x /= not (isSpace y))
-- "Hello World!\na" ~> "olleH !dlroW\na"

-- best an library
-- import Data.List.Split (split,oneOf)
-- reverseWords xs = concatMap reverse (split (oneOf " ") xs)
