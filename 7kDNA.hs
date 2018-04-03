module Codewars.Kata.DNA where

-- import           Codewars.Kata.DNA.Types

-- dnaStrand []        `shouldBe` []
-- dnaStrand [A,T,G,C] `shouldBe` [T,A,C,G]
-- dnaStrand [G,T,A,T] `shouldBe` [C,A,T,A]
-- dnaStrand [A,A,A,A] `shouldBe` [T,T,T,T]

data Base = A | T | G | C deriving(Eq,Show)
type DNA = [Base]

dnaStrand :: DNA -> DNA
dnaStrand = map exchanger where
  exchanger x | x == A    = T
              | x == T    = A
              | x == G    = C
              | otherwise = G

-- or like this
-- dnaStrand = map $ \b -> case b of
  -- A -> T
  -- T -> A
  -- C -> G
  -- G -> C
