module A9n where

import           Data.Char
import           Data.List.Split

-- abbreviate("elephant-rides are really fun!")
--             ^^^^^^^^*^^^^^*^^^*^^^^^^*^^^*
-- words (^):   "elephant" "rides" "are" "really" "fun"
--                123456     123     1     1234     1
-- ignore short words:               X              X

-- abbreviate:    "e6t"     "r3s"  "are"  "r4y"   "fun"
-- all non-word characters (*) remain in place
--                      "-"      " "    " "     " "     "!"
-- === "e6t-r3s are r4y fun!"

-- abbreviate "internationalization" `shouldBe` "i18n"
-- abbreviate "Accessibility" `shouldBe` "A11y"
-- abbreviate "accessibility" `shouldBe` "a11y"
-- abbreviate "elephant-ride" `shouldBe` "e6t-r2e"

-- expected: "1aaa"
--  but got: "12a"
--  "1aaa"
-- " " ==> " "
-- expected: "aaa1"
--  but got: "a21"
--  "aaa1"
-- expected: "aa1a"
--  but got: "a2a"

abbreviate :: String -> String
abbreviate = concatMap (concatMap w2d . split (whenElt $ not . isAlpha)) . split (oneOf " ")

-- best
-- abbreviate = concatMap w2d . split (whenElt (not . isLetter))
-- or
-- abbreviate = concat . map w2d . groupBy (\a b -> isAlpha a && isAlpha b)

w2d :: String -> String
w2d s | null s = s
      | length s <= 3 = s
      | otherwise   = head s : show (length s - 2) ++ drop (length s - 1) s
