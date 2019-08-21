module Engine.Ray (
  Ray(Ray)
, rayLine
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
    rayLine :: Line,
    rayIntensity :: Double,
    rayWavelength :: Wavelength
} deriving (Show, Eq)

geometricRay :: Line -> Ray
geometricRay l = Ray {
    rayLine = l,
    rayIntensity = 1.0,
    rayWavelength = greenYellowWave
}
