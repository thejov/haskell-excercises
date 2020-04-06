import Data.List

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
data Label = A | B | C deriving (Show)
type Route = [(Label, Int)]

shortestRoute :: [Section] -> Route
shortestRoute = reverse . pickShorterRoute . foldl shortestRouteInSection ([],[])

shortestRouteInSection :: (Route, Route) -> Section -> (Route,Route)
shortestRouteInSection (routeToA, routeToB) (Section a b c) = 
  (pickShorterRoute ((A,a):routeToA, (C,c):(B,b):routeToB), pickShorterRoute ((B,b):routeToB, (C,c):(A,a):routeToA))

pickShorterRoute :: (Route,Route) -> Route
pickShorterRoute (routeA,routeB)
  | sum (map snd routeA) < sum (map snd routeB) = routeA
  | otherwise = routeB

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      sections = map (\[a,b,c] -> (Section a b c)) threes
      route = shortestRoute sections
      routeString = concat $ map (show . fst) route
      routeTime = sum $ map snd route
  putStrLn $ "The shortest route to take is: " ++ routeString
  putStrLn $ "Time taken: " ++ show routeTime

exampleSections =
  [
    Section 50 10 30,
    Section 5 90 20,
    Section 40 2 25,
    Section 10 8 0
  ]

shortest = shortestRoute exampleSections