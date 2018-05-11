module Codewars.Parentheses where

import           Data.List.Split

-- "()"              =>  true
-- ")(()))"          =>  false
-- "("               =>  false
-- "(())((()())())"  =>  true
-- "(())()()))())))(())))()()(((()()(((()(()" => false

validParentheses :: String -> Bool
validParentheses []  = True
validParentheses xs | dp == xs = False
                    | otherwise = validParentheses dp
  where
    cs = clear xs
    dp = concat $ splitOn "()" cs

clear :: String -> String
clear = filter (`elem` "()")
