module Maskify where

-- maskify "4556364607935616" == "############5616"

maskify :: String -> String
maskify str | l > 4 = map (const '#') (take l str) ++ drop l str
            | otherwise = str
            where
              l = length str - 4

-- best
maskify2 str = replicate l '#' ++ drop l str
    where l = length str - 4

-- or in another way
maskify3 s
  | length s < 5 = s
  | otherwise = "#" ++ maskify3 (tail s)
