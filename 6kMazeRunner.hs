module Haskell.SylarDoom.MazeRunner where

-- maze = [[1,1,1,1,1,1,1],
--         [1,0,0,0,0,0,3],
--         [1,0,1,0,1,0,1],
--         [0,0,1,0,0,0,1],
--         [1,0,1,0,1,0,1],
--         [1,0,0,0,0,0,1],
--         [1,2,1,0,1,0,1]]

-- mazeX :: [[Int]]
-- mazeX = [[1,1,1,1,1,1,1],[1,0,0,0,0,0,3],[1,0,1,0,1,0,1],[0,0,1,0,0,0,1],[1,0,1,0,1,0,1],[1,0,0,0,0,0,1],[1,2,1,0,1,0,1]]

-- dirX3 :: String
-- dirX3 = ['N','N','N','N','N','E','E','E','E','E','W','W']

-- dirX4 = ['N','N','N','W','W']

-- dirX :: String
-- dirX = ['N','N','N','N','N','E','E','E','E','E','E']

-- dirX2 = ['N','N','N','N','N','E','E','E','N','S','E','E']

-- ['N','N','N','N','N','E','E','E','E','E'] `shouldBe` "Finish"
-- ['N','N','N','W','W'] `shouldBe` "Dead"
-- ['N','E','E','E','E'] `shouldBe` "Lost"


mazeRunner :: [[Int]] -> String -> String
mazeRunner maze dir = case event path of
                  3 -> "Finish"
                  1 -> "Dead"
                  _ -> "Lost"
  where
    path = map (`indXelem` maze) (reverse $ foldl (\acc x -> crem x (head acc) : acc) [mazeStart] dir)
    crem 'N' (x,y) = (x - 1, y)
    crem 'S' (x,y) = (x + 1, y)
    crem 'W' (x,y) = (x, y - 1)
    crem 'E' (x,y) = (x, y + 1)
    mazeStart = elemIndX 2 maze
    event [] = 0
    event (x:xs)  | x == 3 || x == 1 = x
                  | otherwise = event xs

indX :: [[Int]] -> [((Int, Int), Int)]
indX maze = zip [(x,y) | x <- [1..l], y <- [1..l]] mazeC
    where l = length maze
          mazeC = concat maze

elemIndX :: Int -> [[Int]] -> (Int, Int)
elemIndX x maze = fst . head $ filter ((==x) . snd) (indX maze) -- elemIndX 2 maze ~> (7,2)

indXelem :: (Int, Int) -> [[Int]] -> Int -- indXelem (2,7) maze ~> [((2,7),2)]
indXelem (x,y) maze | [] == conv = 1
                    | otherwise = snd . head $ conv
                      where conv = filter ((==(x,y)) . fst) (indX maze)

