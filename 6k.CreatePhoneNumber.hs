module CreatePhoneNumber where

import Data.List
import Data.Char

-- createPhoneNumber [1,2,3,4,5,6,7,8,9,0] -- => returns "(123) 456-7890"
createPhoneNumber :: [Int] -> String
createPhoneNumber list = concat ["(", splitter (<4) xs, ") ", splitter (\x -> (x<7) && (x>3)) xs, "-", splitter (>6) xs]
  where
    splitter f ys = map (intToDigit . fst) $ fst $ partition (f . snd) ys
    xs = zip list [(1::Int)..]
