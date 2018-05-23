module Codewars.Kata.Bookseller where

-- import           Codewars.Kata.Bookseller.Types

st = [Stock "ABAR" 0, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]
-- stocklist stock ['A','B'] `shouldBe` [('A',200), ('B',1140)]
cs = "ABCX"

data Stock = Stock String Int deriving (Show, Eq)

stocklist :: [Stock] -> String -> [(Char, Int)]
stocklist [] cs = [] -- stocklist [] = const []
stocklist st cs = map (\ x -> (x, sum $ map quan $ filter ((==x) . code) st)) cs

code (Stock xs _) = head xs

quan (Stock _ n) = n
