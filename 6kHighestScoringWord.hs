module High.JorgeVS.Kata where

import           Data.List
import           Data.Maybe

-- "man i need a taxi up to ubud" `shouldBe` "taxi"
-- "what time are we climbing up the volcano" `shouldBe` "volcano"

high :: String -> String
high [] = []
high str = snd $ maximum $ zip (map (sum . map (\x -> (+1) . fromJust . elemIndex x $ ['a'..'z'])) li) li
  where li = words str


-- variant
-- high "" = ""
-- high myStr = maximumBy (compare `on` value) $ words myStr
-- where
  -- value s = sum . map ((subtract 96) . ord) $ s
