module AlternateSplit.JorgeVS.Kata where

import           Data.List (partition, transpose)

-- "This is a test!", 1 -> "hsi  etTi sats!"
-- "This is a test!", 2 -> "hsi  etTi sats!" -> "s eT ashi tist!"
encrypt :: String -> Int -> String
encrypt text n | n <= 0    = text
               | otherwise = con (encrypt text (n-1))

decrypt :: String -> Int -> String
decrypt text n  | n <= 0    = text
                | otherwise = rcon (decrypt text (n-1))


split :: [b] -> ([b], [b])
split xs =
  let (a,b) = partition (even . snd) (zip xs [(1::Integer)..])
  in (map fst a, map fst b)

con :: [a] -> [a]
con list = uncurry (++) (split list)

splitHalf :: [a] -> ([a], [a])
splitHalf list = splitAt (length list `div` 2) list

trans :: ([a], [a]) -> [[a]]
trans xs = transpose [snd xs, fst xs]

rcon :: [a] -> [a]
rcon = concat . trans . splitHalf

x1 = encrypt "This is a test!" 1
x2 = encrypt "This kata is very interesting!" 1
y1 = "This is a test!"
y2 = "This kata is very interesting!"

