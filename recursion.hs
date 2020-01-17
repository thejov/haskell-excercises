maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Cannot take maximum of an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Int) -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x
