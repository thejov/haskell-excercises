import Data.Monoid

-- Get Ordering of string lengths
-- or the Ordering based on amount of vowels in the strings if lengths are equal
-- or alphabetical Orderding if the vocel counts are equal
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
     where vowels = length . filter (`elem` "aeiou")
