module CamelCase.JorgeVS.Kata where

import           Data.Char (toUpper)
import           Data.List (words)

--"camel case word".camelCase() => CamelCaseWord
camelCase :: String -> String
camelCase str = concatMap (\(x:xs) -> toUpper x : xs) (words str)

-- x = "hello world"
