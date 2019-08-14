module Ray (
  Ray(Ray),
) where

import Angle
import Point

data Ray = Ray Point Angle deriving (Show, Eq)


