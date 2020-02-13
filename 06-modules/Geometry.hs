module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidVolume
, cuboidArea
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0/3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume side1 side2 side3 = side1 * side2 * side3

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea side1 side2 side3 = 2 * ( side1 * side2 + side1 * side3 + side2 * side3 )
