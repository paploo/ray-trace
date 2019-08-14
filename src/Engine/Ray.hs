module Engine.Ray (
  Ray(Ray),
  origin,
  direction,
  intensity,
  wavelength
) where

import Engine.Angle
import Engine.Point
import Engine.Wavelength

data Ray = Ray {
    origin :: Point,
    direction :: Angle,
    intensity :: Double,
    wavelength :: Wavelength
} deriving (Show, Eq)

simpleRay :: Point -> Angle -> Ray
simpleRay o a = Ray {
    origin = o,
    direction = a,
    intensity = 1.0,
    wavelength = greenYellow
}
