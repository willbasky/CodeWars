module Disemvowel where

disemvowel :: String -> String
disemvowel [] = []
disemvowel (x:xs)
  | notVowel x = x : disemvowel xs
  | otherwise = disemvowel xs

notVowel :: Char -> Bool
notVowel c = c `notElem` "aeiouAEIOU"


-- а можно было так
-- disemvowel = filter (`notElem` "AEIOUaeiou")
