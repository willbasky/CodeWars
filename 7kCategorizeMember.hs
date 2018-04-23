module Codewars.Kata.Categorize where
-- import Codewars.Kata.Categorize.Types

-- openOrSenior [(55,7),(55,8),(54,9)] `shouldBe` [Open, Senior, Open]

data Membership = Open | Senior deriving (Eq, Show)

openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior = map (\(x,y) -> if x >= 55 && y > 7 then Senior else Open)
