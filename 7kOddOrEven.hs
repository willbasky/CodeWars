module Kata.OddOrEven where

-- oddOrEven([0]) returns "even"
-- oddOrEven([2, 5, 34, 6]) returns "odd"
-- oddOrEven([0, -1, -5]) returns "even"

oddOrEven :: Integral a => [a] -> String
oddOrEven xs  | even (sum xs) = "even"
              | otherwise     = "odd"


-- best
-- import Data.Bool
-- >>> bool "foo" "bar" True ~> "bar"
-- >>> bool "foo" "bar" False ~> "foo"
-- oddOrEven2 = bool "odd" "even" . even . sum
