module WeIrDStRiNgCaSe where

import Data.List
import Data.Char

-- toWeirdCase "String"            `shouldBe` "StRiNg"
-- toWeirdCase "Weird string case" `shouldBe` "WeIrD StRiNg CaSe"
toWeirdCase :: String -> String
toWeirdCase = unwords . map rcon . words

split :: String -> (String, String)
split xs = let (a,b) = partition (odd . snd) (zip xs [(1::Int)..])
  in (map (toUpper . fst) a, map (toLower . fst) b)

trans :: ([a], [a]) -> [[a]]
trans xs = transpose [fst xs, snd xs]

rcon :: String -> String
rcon = concat . trans . split

-- best
toWeirdCase2 :: String -> String
toWeirdCase2 = unwords . map (zipWith ($) weird) . words
  where weird = cycle [toUpper, toLower]

weird2 :: [Char -> Char]
weird2 = cycle [toUpper, toLower]
