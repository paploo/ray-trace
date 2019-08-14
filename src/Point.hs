module Point (
    Point(Point)
) where

data Point = Point Double Double deriving (Show, Eq)
