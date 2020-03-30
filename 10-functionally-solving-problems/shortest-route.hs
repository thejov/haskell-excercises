type Section = (Int,Int,Int)
type Route = [Int]

shortestRoute :: [Section] -> Route
shortestRoute = pickShorterRoute . foldl shortestRouteInSection ([],[])

shortestRouteInSection :: (Route, Route) -> Section -> (Route,Route)
shortestRouteInSection (routeToA, routeToB) (a,b,c) = 
  (pickShorterRoute (routeToA++[a], routeToB ++ [b] ++ [c]), pickShorterRoute (routeToB++[b], routeToA ++ [a] ++ [c]))

pickShorterRoute :: (Route,Route) -> Route
pickShorterRoute (routeA,routeB)
  | sum routeA < sum routeB = routeA
  | otherwise               = routeB

exampleSections =
  [
    (50,10,30),
    (5,90,20),
    (40,2,25),
    (10,8,0)
  ]

shortest = shortestRoute exampleSections