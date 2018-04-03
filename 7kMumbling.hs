module Codewars.G964.Accumule where

import Data.Char
import Data.List
-- accum "abcd"    -- "A-Bb-Ccc-Dddd"
-- accum "RqaEzty" -- "R-Qq-Aaa-Eeee-Zzzzz-Tttttt-Yyyyyyy"
-- accum "cwAt"    -- "C-Ww-Aaa-Tttt"
accum :: String -> String
accum s = intercalate "-" $ map (\x -> toUpper (fst x) : replicate (snd x) (toLower $ fst x)) $ zip s [0..]
