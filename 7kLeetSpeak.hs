module LeetSpeak where

-- toLeetSpeak "LEET" `shouldBe` "1337"
-- toLeetSpeak "CODEWARS" `shouldBe` "(0D3W@R$"
-- toLeetSpeak "HELLO WORLD" `shouldBe` "#3110 W0R1D"
-- toLeetSpeak "LOREM IPSUM DOLOR SIT AMET" `shouldBe` "10R3M !P$UM D010R $!7 @M37"
-- toLeetSpeak "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" `shouldBe` "7#3 QU!(K 8R0WN F0X JUMP$ 0V3R 7#3 1@2Y D06"

import           Data.Maybe

toLeetSpeak :: String -> String
toLeetSpeak = unwords . map (map (fromJust . (`lookup` dict))) .  words

dict :: [(Char, Char)]
dict = [
  ('A', '@'),
  ('B', '8'),
  ('C', '('),
  ('D', 'D'),
  ('E', '3'),
  ('F', 'F'),
  ('G', '6'),
  ('H', '#'),
  ('I', '!'),
  ('J', 'J'),
  ('K', 'K'),
  ('L', '1'),
  ('M', 'M'),
  ('N', 'N'),
  ('O', '0'),
  ('P', 'P'),
  ('Q', 'Q'),
  ('R', 'R'),
  ('S', '$'),
  ('T', '7'),
  ('U', 'U'),
  ('V', 'V'),
  ('W', 'W'),
  ('X', 'X'),
  ('Y', 'Y'),
  ('Z', '2')
  ]
