module Engine.Wavelength (
    Wavelength(Wavelength),
    microns,
    greenYellow
) where

newtype Wavelength = Wavelength { microns :: Double } deriving (Show, Eq, Ord)

greenYellow :: Wavelength
greenYellow = Wavelength 0.550
