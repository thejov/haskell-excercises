zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b  -> c) -> (b -> a -> c)
flip' f a b = f b a

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<= x) xs) ++ [x] ++ quicksort (filter (> x) xs)

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let smallerOrEqual = filter (<= x) xs
        greater = filter (> x) xs
    in quicksort' smallerOrEqual ++ [x] ++ quicksort' greater

largestDivisble :: Integer
largestDivisble = head (filter p [99999,99998..])
  where p x = (x `mod` 3829) == 0

collatzSequenceOf :: Integer -> [Integer]
collatzSequenceOf 1 = [1]
collatzSequenceOf x
    | even x    = x : collatzSequenceOf (x `div` 2)
    | otherwise = x : collatzSequenceOf (x*3+1)

numLongCollatzSequences :: Int
numLongCollatzSequences = length (filter isLong (map collatzSequenceOf [1..100]))
    where isLong xs = length xs > 15

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\acc x -> acc ++ [f x]) []

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldr (\y acc -> (y == x) || acc) False

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\a acc -> if f a then a : acc else acc ) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)
