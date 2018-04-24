module OddNotPrime where

-- oddNotPrime 5 `shouldBe` 1
-- oddNotPrime 10 `shouldBe` 2
-- oddNotPrime 99 `shouldBe` 26

-- import           Data.Numbers.Primes
import           Data.List
import qualified Data.Set  as PQ

oddNotPrime :: Int -> Int
oddNotPrime n = length ((\\) [1,3..n] (takeWhile (<=n) primes))

primes :: [Int]
primes = sieve [3,5..]
  where
    sieve (x:xs) = x : sieve' xs (insertprime x xs PQ.empty)

    sieve' (x:xs) table
        | nextComposite == x = sieve' xs (adjust x table)
        | otherwise          = x : sieve' xs (insertprime x xs table)
      where
        (nextComposite,_) = PQ.findMin table

    adjust x table
        | n == x    = adjust x (PQ.insert (n', ns) newPQ)
        | otherwise = table
      where
        Just ((n, n':ns), newPQ) = PQ.minView table

    insertprime p xs = PQ.insert (p*p, map (*p) xs)


-- this library didn`t works on code wars tester.
-- import           Data.Numbers.Primes

-- oddNotPrime :: Int -> Int
-- oddNotPrime n = length [1, 3 .. n] - length (filter isPrime [1,3..9])

-- best and better solution by perfomance
oddNotPrime2 :: Int -> Int
oddNotPrime2 n = 1 + length [m | m <- [1, 3 .. n], any ((== 0) . mod m) [3, 5 .. floor $ sqrt $ fromIntegral m]]


