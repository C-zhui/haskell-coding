
data Point = Point Float Float deriving(Show)

data Shape = Circle Point Float | Rectangle Point Point deriving(Show)

area :: Shape -> Float 
area (Circle p r) = pi * r * r
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

radius = [1..10]
circles = map (Circle (Point 0 0) ) radius

ends = zip [1..10] [10,9..1]
rectangles = map (\t -> Rectangle (Point 0 0) (Point (fst t)(snd t))) ends

a1 = map area circles 
a2 = map area  rectangles 
a3 = map area $ circles ++ rectangles

