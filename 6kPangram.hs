module Pangram where

import           Data.Char
import           Data.List
import           Data.List.Split

-- "The quick brown fox jumps over the lazy dog." ~~> True

isPangram :: String -> Bool
isPangram s = ['a'..'z'] == (sort . nub . concat . splitWhen (not . isAlpha) . map toLower $ s)


-- shorter
isPangram2 str = all (`elem` map toLower str) ['a'..'z']
