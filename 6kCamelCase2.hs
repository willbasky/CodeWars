module CamelCase where

import           Data.Char
import           Data.List.Split

-- toCamelCase "the-stealth-warrior" -- returns "theStealthWarrior"
-- toCamelCase "The_Stealth_Warrior" -- returns "TheStealthWarrior"

toCamelCase :: String -> String
toCamelCase [] = []
toCamelCase "_" = []
toCamelCase "-" = []
toCamelCase str = concat $ x : map (\(y:ys) -> toUpper y : ys) xs
  where
    dropper = dropDelims . dropBlanks $ oneOf "_-"
    (x:xs) = case str of
      ('_':_) -> "" : split dropper str
      ('-':_) -> "" : split dropper str
      (_:_)   -> split dropper str


--best
capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

toCamelCase2 :: String -> String
toCamelCase2 str = concat $ head words : map capitalize (tail words)
  where words = splitOneOf "-_" str
