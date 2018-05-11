module Codewars.Kata.Rectangle where

-- squaresInRect  5  3 `shouldBe` Just [3,2,1,1]
-- squaresInRect  3  5 `shouldBe` Just [3,2,1,1]
-- squaresInRect 20 14 `shouldBe` Just [14, 6, 6, 2, 2, 2]

squaresInRect :: Integer -> Integer -> Maybe [Integer]
squaresInRect lng wdth  | lng == wdth = Nothing
                        | otherwise   = Just $ go (max lng wdth) (min lng wdth)
  where
    go _ 0 = []
    go a b = b : go (max b (a - b)) (min b (a - b))
