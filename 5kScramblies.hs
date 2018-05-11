module Codewars.G964.Scramblies where

import           Data.List

-- scramble('rkqodlw', 'world') ==> True
-- scramble('cedewaraaossoqqyt', 'codewars') ==> True
-- scramble('katas', 'steak') ==> False
--  s1: scriptjavx, s2: javascript ==> False
--  s1: javscripts, s2: javascript  ==> False
-- "bdamaebhwbmnhzqunuzjgeljltbp" "nkycvdrisfruehzlaojqdpomgdw" ==> False
-- "pkewxlatvnhphgscnbmpcgcwhvjgmzmfetxu" "ecyibkysgorckvsbrnzyjfurc"


scramble :: String -> String -> Bool
scramble s1 s2 = map nub r1 == map nub r2 && map length r1 >= map length r2
    where
      r1 = group $ sort $ filter (`elem` s2) s1
      r2 = group (sort s2)


w1 x1 x2 = group $ sort $ filter (`elem` x2) x1
w2 x2 = group $ sort x2
eq a1 a2= filter (`elem` a2) a1

g1 = "rkqoldw"
g2 = "world"
t1 = "scriptjavx"
t2 = "javascript"
b1 = "javscripts"
b2 = "javascript"
d1 = "cedewaraaossoqqyt"
d2 = "codewars"
k1 = "bdamaebhwbmnhzqunuzjgeljltbp"
k2 = "nkycvdrisfruehzlaojqdpomgdw"
p1 = "ybqiboccfwaehhvbfgipuurmzmmitoav"
p2 = "niigwlysqyvjfnxmcfuwvqqgnerenhybjblqwg"
