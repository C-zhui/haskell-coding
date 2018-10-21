data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving(Show)

area :: Shape -> Float 
area (Circle _  _  r) = pi * r * r
area (Rectangle x1 y1 x2 y2 ) = (abs $ x1 - x2) * (abs $ y1 - y2)

radius = [1..10]
circles = map (Circle 1 2) radius

ends = zip [1..10] [10,9..1]
rectangles = map (\t -> Rectangle 0 0 (fst t) (snd t)) ends

a1 = map area circles 
a2 = map area rectangles 
a3 = map area $ circles ++ rectangles

