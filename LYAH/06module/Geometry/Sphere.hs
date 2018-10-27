module Geometry.Sphere
(
    volume,area
)where

volume :: Float -> Float
volume r = (r ^ 3) * pi * 4 / 3 

area:: Float -> Float 
area r = 4 * pi * (r ^ 2)

