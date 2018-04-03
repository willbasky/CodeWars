module Codewars.Kata.Vowel where

getCount :: String -> Int
getCount str = length $ filter (\c -> c `elem` ['a','e','i','o','u']) str
