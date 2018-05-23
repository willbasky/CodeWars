module Codewars.Kata.Arithmetic where

-- findMissing [1,2,3,4,6]  `shouldBe` 5
-- findMissing [1,3,5,9]    `shouldBe` 7
-- findMissing [1,2,4,5]    `shouldBe` 3
-- findMissing [1,2,4]      `shouldBe` 3
-- findMissing [1,3,4]      `shouldBe` 2
-- findMissing [1,3,5,9,11] `shouldBe` 7

findMissing :: Integral n => [n] -> n
findMissing = divi

divi :: (Integral t) => [t] -> t
divi []    = 0
divi [_,_] = 0
divi (x:y:z:zs) | y - x == z - y = divi (y:z:zs)
                | otherwise = if (y - x) > (z - y)
                                  then x + div (y - x) 2
                                  else y + div (z - y) 2

-- best
-- findMissing (x:y:z:r)
  -- | y - x == z - y = findMissing (y:z:r)
  -- | otherwise = x + z - y
