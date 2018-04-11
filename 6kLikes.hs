module Likes where

-- likes [] // must be "no one likes this"
-- likes ["Peter"] // must be "Peter likes this"
-- likes ["Jacob", "Alex"] // must be "Jacob and Alex like this"
-- likes ["Max", "John", "Mark"] // must be "Max, John and Mark like this"
-- likes ["Alex", "Jacob", "Mark", "Max"] // must be "Alex, Jacob and 2 others like this"

likes :: [String] -> String
likes [] = "no one likes this"
likes [x] = x ++ " likes this"
likes [x,y] = x ++ " and " ++ y ++ " like this"
likes [x,y,z] = x ++ ", " ++ y ++ " and " ++ z ++ " like this"
likes (s:d:xs) = s ++ ", " ++ d ++ " and " ++ show (length xs) ++ " others" ++ " like this"
