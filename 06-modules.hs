import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

numEachWordInString :: String -> [(String, Int)]
numEachWordInString = map (\word -> (head word,length word)) . group . sort . words

isContainedIn :: (Eq a) => [a] -> [a] -> Bool
isContainedIn xs ys = any (isPrefixOf xs) (tails ys)

rotEncode :: Int -> String -> String
rotEncode n = map (chr . (+n) . ord)

rotDecode :: Int -> String -> String
rotDecode n = rotEncode (-n)

sumDigits :: Int -> Int
sumDigits = sum . map digitToInt . show

firstSumDigitEqual40 :: Int
firstSumDigitEqual40 = head [ x | x <- [1..], sumDigits x == 40 ]

firstSumDigitEqual40' :: Maybe Int
firstSumDigitEqual40' = find (\x -> sumDigits x == 40) [1..]

firstSumDigitEqual40'' :: Maybe Int
firstSumDigitEqual40'' = find ((==40) . sumDigits) [1..]

firstSumDigitEqual :: Int -> Maybe Int
firstSumDigitEqual n = find ((==n) . sumDigits) [1..]

findTuple :: (Eq a) => a -> [(a,b)] -> Maybe (a,b)
findTuple x = find (\(y,_) -> y==x)

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey x = snd . head . filter (\(y,_) -> x==y)

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' y [] = Nothing
findKey' y ((k,v):xs)
    | y == k = Just v
    | otherwise = findKey' y xs 

findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' y = foldr (\(k,v) acc -> if k == y then Just v else acc) Nothing
