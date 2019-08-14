module Engine.Ray (
  Ray(Ray),
) where

import Engine.Angle
import Engine.Point

data Ray = Ray Point Angle deriving (Show, Eq)


