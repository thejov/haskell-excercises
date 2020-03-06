main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map isPalindrome . lines

isPalindrome :: String -> String
isPalindrome xs
    | xs == reverse xs = "palindrome"
    | otherwise = "not a palindrome"
