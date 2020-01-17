maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot take maximum of an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)