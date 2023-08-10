module Main where

import           Criterion.Main
import           Data.List

-- findMissing [1,2,3,4,6]  `shouldBe` 5
-- findMissing [1,3,5,9]    `shouldBe` 7
-- findMissing [1,2,4,5]    `shouldBe` 3
-- findMissing [1,2,4]      `shouldBe` 3
-- findMissing [1,3,4]      `shouldBe` 2
-- findMissing [1,3,5,9,11] `shouldBe` 7


-- fib m | m < 0     = error "negative!"
--       | otherwise = go m
--   where
--     go 0 = 0
--     go 1 = 1
--     go n = go (n-1) + go (n-2)

-- -- Our benchmark harness.
-- main = defaultMain [
--   bgroup "fib"  [ bench "1"  $ whnf fib 1
--                 , bench "5"  $ whnf fib 5
--                 , bench "9"  $ whnf fib 9
--                 , bench "11" $ whnf fib 11
--                 ]
--   ]

findMissing :: Integral n => [n] -> n
findMissing = (\(x,y) -> y + div x 2) . maximumBy (\x y -> compare (abs . fst $ x) (abs. fst $ y)) . divisio

divisio :: Num a => [a] -> [(a, a)]
divisio []       = []
divisio [x,y]    = [(y-x, x)]
divisio (x:y:ys) = (y-x, x) : divisio (y:ys)

-- Our benchmark harness.
main = defaultMain [
  bgroup "findMissing"  [ bench "[1,2,4]"  $ whnf findMissing [1,2,4]
                        , bench "[1,3,5,9,11]"  $ whnf findMissing [1,3,5,9,11]
                        , bench "[1,3,7,9,11,13,15,17,19,21]"  $ whnf findMissing [1,3,7,9,11,13,15,17,19,21]
                        , bench "[1,3,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41]" $ whnf findMissing [1,3,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41]
                        ]
  ]
-- main = defaultMain [
--   bgroup "divisio"  [ bench "[1,2,4]"  $ whnf divisio [1,2,4]
--                         , bench "[1,3,5,9,11]"  $ whnf divisio [1,3,5,9,11]
--                         , bench "[1,3,7,9,11,13,15,17,19,21]"  $ whnf divisio [1,3,7,9,11,13,15,17,19,21]
--                         -- , bench "11" $ whnf findMissing
--                         ]
--   ]

