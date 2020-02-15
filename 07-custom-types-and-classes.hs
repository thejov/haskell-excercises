data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 -y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) nudgeX nudgeY = Circle (Point (x + nudgeX) (y + nudgeY)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) nudgeX nudgeY
    = Rectangle (Point (x1 + nudgeX) (y1 + nudgeY)) (Point (x2 + nudgeX) (y2 + nudgeY))

origoCircle :: Float -> Shape
origoCircle r = Circle (Point 0 0) r

origoRectangle :: Float -> Float -> Shape
origoRectangle height width = Rectangle (Point 0 0) (Point height width)
