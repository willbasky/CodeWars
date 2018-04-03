module JadenCasing where

import Data.Char

-- Not Jaden-Cased: "How can mirrors be real if our eyes aren't real"
-- Jaden-Cased:     "How Can Mirrors Be Real If Our Eyes Aren't Real"

toJadenCase :: String -> String
toJadenCase = unwords . map (\(x:xs) -> toUpper x : xs) . words
