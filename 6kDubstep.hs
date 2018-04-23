module Codewars.Kata.Dubstep where

import Data.List.Split

--songDecoder "WUBWEWUBAREWUBWUBTHEWUBCHAMPIONSWUBMYWUBFRIENDWUB"
  -- `shouldBe` "WE ARE THE CHAMPIONS MY FRIEND"
songDecoder :: String -> String
songDecoder = unwords . split (dropBlanks . dropDelims $ onSublist "WUB")
