import Data.Monoid

-- Get Ordering of string lengths or alphabetical Orderding if lengths are equal
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)
