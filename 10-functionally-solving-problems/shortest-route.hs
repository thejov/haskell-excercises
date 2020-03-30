type Section = (Int,Int,Int)

shortestRoute :: [Section] -> Section
shortestRoute = foldl shortestRouteInSection (0,0,0)

shortestRouteInSection :: Section -> Section -> Section
shortestRouteInSection (acc_a, acc_b, _) (a,b,c) = (
    min (acc_a+a) (acc_b+b+c),
    min (acc_b+b) (acc_a+a+c),
    0
  )

sections =
  [
    (50,10,30),
    (5,90,20),
    (40,2,25),
    (10,8,0)
  ]

shortest = shortestRoute sections