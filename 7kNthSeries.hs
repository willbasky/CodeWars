module Codewars.Kata.NthSeries where

import Numeric
-- Series: 1 + 1/4 + 1/7 + 1/10 + 1/13 + 1/16 +...
-- SeriesSum(1) => 1 = "1.00"
-- SeriesSum(2) => 1 + 1/4 = "1.25"
-- SeriesSum(5) => 1 + 1/4 + 1/7 + 1/10 + 1/13 = "1.57"

seriesSum :: Integer -> String
seriesSum 0 = "0.00"
seriesSum n = showGFloat (Just 2) ((+) 1 . sum . take (fromIntegral n-1) . map (1 /) $ [4, 7..]) ""


-- without Numeric library
-- seriesSum n = show . (/ 100) . fromIntegral . round . (*) 100 . (+) 1 . sum . take (fromIntegral n-1) . map (1 /) $ [4, 7..]
