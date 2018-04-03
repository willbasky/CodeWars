-- module Codewars.Kata.DecodeMorse (decodeMorse) where
module Codewars.Kata.DecodeMorse where

-- import Codewars.Kata.DecodeMorse.Preload (morseCodes)

-- decodeMorse ".... . -.--   .--- ..- -.. ."
-- should return "HEY JUDE"

import Data.Map.Strict ((!), member, fromList)
import Data.List.Split
import Data.Maybe

decodeMorse :: String -> String
decodeMorse = unwords . map (concatMap convert . splitter " ") . filter (/="  ") . splitter "   "
  where
    splitter x = split (dropBlanks . dropDelims $ onSublist x)
    convert y = if member y dictanary then dictanary ! y else ""
  -- (!?) этот оператор не импортируется в тесте на кодварс.
  -- convert y = if isJust (dictanary !? y) then fromJust $ dictanary !? y else " "

-- best. But it return error if wrong key
decodeMorse2 = unwords . filter (not . null) . map (concatMap (dictanary!) . words) . splitOn "   "

-- Нужно для написания кода здесь. На кодеварс подключается сет fromList morseCodes.
dictanary = fromList [(".−", "A"),
                      ("-...", "B"),
                      ("-.-.", "C"),
                      ("-..", "D"),
                      (".", "E"),
                      ("..-.", "F"),
                      ("--.", "G"),
                      ("....", "H"),
                      ("..", "I"),
                      (".---", "J"),
                      ("-.-", "K"),
                      (".-..", "L"),
                      ("--", "M"),
                      ("-.", "N"),
                      ("---", "O"),
                      (".--.", "P"),
                      ("--.-", "Q"),
                      (".-.", "R"),
                      ("...", "S"),
                      ("-", "T"),
                      ("..-", "U"),
                      ("...-", "V"),
                      (".--", "W"),
                      ("-..-", "X"),
                      ("-.--", "Y"),
                      ("--..", "Z"),
                      ("-----", "0"),
                      (".----", "1"),
                      ("..---", "2"),
                      ("...--", "3"),
                      ("....-", "4"),
                      (".....", "5"),
                      ("-....", "6"),
                      ("--...", "7"),
                      ("---..", "8"),
                      ("----.", "9"),
                      (".-.-.-", "."),
                      ("--..--", ","),
                      ("..--..", "?"),
                      (".----.", "'"),
                      ("-.-.--", "!"),
                      ("-..-.", "/"),
                      ("-.--.", "("),
                      ("-.--.-", ")"),
                      (".-...", "&"),
                      ("---...", ":"),
                      ("-.-.-.", ";"),
                      ("-...-", "="),
                      (".-.-.", "+"),
                      ("-....-", "-"),
                      ("..--.-", "_"),
                      (".-..-.", "\""),
                      ("...-..-", "$"),
                      (".--.-.", "@"),
                      ("", ""),
                      ("", "")]
