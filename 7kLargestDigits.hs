module LargestDigits where

-- digit5 "7316717"       `shouldBe` 73167
-- digit5 "1234567898765" `shouldBe` 98765

digit5 :: String -> Int
digit5 = read . maximum . filter (\ x -> length x == 5) . list5 where
    list5 [] = []
    list5 ys = take 5 ys : list5 (drop 1 ys)

