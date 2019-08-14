module Angle (
    Angle(Angle),
    radians,
    fromDegrees,
    degrees
) where

newtype Angle = Angle { radians :: Double } deriving (Show, Eq, Ord)

fromDegrees :: Double -> Angle
fromDegrees a = Angle (a * pi / 180.0)

degrees :: Angle -> Double
degrees (Angle a) = a * 180.0 / pi


