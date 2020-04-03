type Section = (Int, Int, Int)
data Label = A | B | C deriving (Show)
type Route = [(Label, Int)]

shortestRoute :: [Section] -> Route
shortestRoute = reverse . pickShorterRoute . foldl shortestRouteInSection ([],[])

shortestRouteInSection :: (Route, Route) -> Section -> (Route,Route)
shortestRouteInSection (routeToA, routeToB) (a,b,c) = 
  (pickShorterRoute ((A,a):routeToA, (C,c):(B,b):routeToB), pickShorterRoute ((B,b):routeToB, (C,c):(A,a):routeToA))

pickShorterRoute :: (Route,Route) -> Route
pickShorterRoute (routeA,routeB)
  | sum (map snd routeA) < sum (map snd routeB) = routeA
  | otherwise               = routeB

exampleSections =
  [
    (50,10,30),
    (5,90,20),
    (40,2,25),
    (10,8,0)
  ]

shortest = shortestRoute exampleSections