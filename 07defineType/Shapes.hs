module Shapes(
    Point(..),
    Shape(..),
    baseCircle,
    baseRectangle,
    nudge,
    area,
)where

data Point = Point Float Float deriving(Show)

data Shape = Circle Point Float | Rectangle Point Point deriving(Show)

nudge (Circle (Point x1 y1) r) x2 y2 = Circle (Point (x1+x2) (y1+y2))  r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =  (Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b)))

baseCircle r = Circle (Point 0 0) r
baseRectangle a b = Rectangle (Point 0 0) (Point a b)

area :: Shape -> Float 
area (Circle p r) = pi * r * r
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

radius = [1..10]
circles = map  baseCircle  radius

ends = zip [1..10] [10,9..1]
rectangles = map (\t -> baseRectangle (fst t) (snd t)) ends

a1 = map area circles 
a2 = map area  rectangles 
a3 = map area $ circles ++ rectangles

