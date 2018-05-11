module Codewars.Kata.LongestPalindrome where

-- "a" -> 1
-- "aab" -> 2
-- "abcde" -> 1
-- "zzbaabcd" -> 4
-- "" -> 0

longestPalindrome :: Eq a => [a] -> Int
longestPalindrome [] = 0
longestPalindrome n = maximum . map length $ palist n []
  where
    palist [] x = x
    palist y x  = palist (fst (palSplit y)) (snd (palSplit y) : x)

palSplit :: Eq a => [a] -> ([a], [a])
palSplit = spanList (\x -> x /= reverse x)

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
        then (x:ys,zs)
        else ([],list)
    where (ys,zs) = spanList func xs


-- longestPalindrome n = palist n
--   where
--     palist = (\(x,y) -> if null x || null y then maxLength [x,y] else palist (uncurry (\a b -> maxLength [a, b]) (x,y))) . breakList is_palindrome

-- uncurry (\x y -> maximum [x,y]) ("123","1")
-- [[a]] -> [a]
maxLength :: Ord a => [[a]] -> [a]
maxLength = snd . maximum . map (\ x -> (length x, x))

-- best
-- import Data.List
-- longestPalindrome :: Eq a => [a] -> Int
-- longestPalindrome = maximum . map length . filter (\s -> s == reverse s) . concatMap tails . inits
-- or better
-- longestPalindrome s = maximum $ map length $ filter (\x -> x == reverse x) $ (inits s) >>= tails
