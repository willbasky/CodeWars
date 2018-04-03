module Codewars.G964.FindEven where

import           Control.Applicative ((<$>), (<*>))
import           Data.List           (elemIndex)
import           Data.Maybe          (fromMaybe)
-- Let's say you are given the array {1,2,3,4,3,2,1}:
-- Your function will return the index 3, because at the 3rd position of the array, the sum of left side of the index ({1,2,3}) and the sum of the right side of the index ({3,2,1}) both equal 6.

findEvenIndex :: [Int] -> Int
findEvenIndex arr = if null solution then -1 else head solution
  where
    solution = filter (/=(-1)) $ foldr (\(x,y) acc -> if boolSum y arr then y:acc else (-1):acc) [] (zip arr [0..])
    boolSum y arr = sum (take y arr) == sum (drop (y+1) arr)

-- best
findEvenIndex2 = fromMaybe (-1) . elemIndex True .
  (zipWith (==) <$> scanl1 (+) <*> scanr1 (+))

-- scanr1 (+)  [20,10,-80,10,10,15,35] ~> [20,0,-10,70,60,50,35]
-- scanl1 (+)  [20,10,-80,10,10,15,35] ~> [20,30,-50,-40,-30,-15,20]
-- (zipWith (==) <$> scanl1 (+) <*> scanr1 (+)) [1,2,3,4,3,2,1] ~> [False,False,False,True,False,False,False]

-- ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."] ~> ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."] 
