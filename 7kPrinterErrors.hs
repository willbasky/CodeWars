module Codewars.G964.Printer where

-- s="aaabbbbhaijjjm"
-- error_printer(s) => "0/14"
-- s="aaaxbbbbyyhwawiwjjjwwm"
-- error_printer(s) => "8/22"

printerError :: String -> String
printerError s = (show . length . filter (`notElem` ['a'..'m']) $ s) ++ "/" ++ show (length s)
