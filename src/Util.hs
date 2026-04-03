module Util (strPadLeft, strPadRight) where

-- | Pad a String on the left with spaces to at least n characters (right-aligns text).
strPadLeft :: Int -> String -> String
strPadLeft n s = replicate (max 0 (n - length s)) ' ' <> s

-- | Pad a String on the right with spaces to at least n characters (left-aligns text).
strPadRight :: Int -> String -> String
strPadRight n s = s <> replicate (max 0 (n - length s)) ' '
