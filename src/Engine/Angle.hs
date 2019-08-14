{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Engine.Angle (
    Angle(Angle),
    radians,
    fromDegrees,
    degrees,
    reverseAngle
) where

--import Engine.Point

newtype Angle = Angle { radians :: Double } deriving (Show, Eq, Ord, Num, Fractional)

fromDegrees :: Double -> Angle
fromDegrees a = Angle (a * pi / 180.0)

degrees :: Angle -> Double
degrees (Angle a) = a * 180.0 / pi

reverseAngle :: Angle -> Angle
reverseAngle (Angle a) = Angle (a - 180.0)

--Not sure if I like having the Point dependency in an otherwise pure implementaiton.
--unitCircle :: Angle -> Point
--unitCircle (Angle a) = Point ((cos a), (sin a))

instance Semigroup Angle where
    (<>) = (+)

instance Monoid Angle where
    mempty = Angle 0.0

