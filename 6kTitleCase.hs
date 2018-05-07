module TitleCase (titleCase) where

import           Data.Char

-- titleCase "a an the of" "a clash of KINGS" -- should return: "A Clash of Kings"
-- titleCase "The In" "THE WIND IN THE WILLOWS" -- should return: "The Wind in the Willows"
-- titleCase "" "the quick brown fox" -- should return: "The Quick Brown Fox"

titleCase :: String -> String -> String
titleCase _ [] = []
titleCase minor title = unwords $ upper (head list) : map (\x -> if x `elem` minorList then lower x else upper x) (tail list)
  where list = words $ lower title
        minorList = words $ lower minor

upper :: String -> String
upper (x:xs) = toUpper x : map toLower xs

lower :: String -> String
lower = map toLower
