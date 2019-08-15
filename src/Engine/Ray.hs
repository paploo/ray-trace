module Engine.Ray (
  Ray(Ray)
, rayOrigin
, rayDirection
, rayIntensity
, rayWavelength
, geometricRay

, Wavelength(Wavelength)
, waveMicrons
, greenYellowWave
) where

import Engine.Geom

--
-- Wavelength
--

newtype Wavelength = Wavelength { waveMicrons :: Double } deriving (Show, Eq, Ord)

greenYellowWave :: Wavelength
greenYellowWave = Wavelength 0.550

--
-- Ray
--

data Ray = Ray {
    rayOrigin :: Point,
    rayDirection :: Angle,
    rayIntensity :: Double,
    rayWavelength :: Wavelength
} deriving (Show, Eq)

geometricRay :: Point -> Angle -> Ray
geometricRay o a = Ray {
    rayOrigin = o,
    rayDirection = a,
    rayIntensity = 1.0,
    rayWavelength = greenYellowWave
}
