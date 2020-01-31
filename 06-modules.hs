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
rotDecode n = map (chr . subtract n . ord)
