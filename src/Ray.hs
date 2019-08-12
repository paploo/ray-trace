module Ray (
  Ray(Ray),
  Point(Point),
  Angle(Radians)
) where

data Ray = Ray Point Angle deriving (Show, Eq)

data Point = Point Double Double deriving (Show, Eq)

xCoord :: Point -> Double
xCoord (Point x _) = x

yCoord :: Point -> Double
yCoord (Point _ y) = y

fromTuple :: (Double, Double) -> Point
fromTuple (x,y) = Point x y

toTuple :: Point -> (Double, Double)
toTuple (Point x y) = (x,y)

data Angle = Radians Double deriving (Show, Eq)

fromDegrees :: Double -> Angle
fromDegrees a = Radians (a * pi / 180.0)

radians :: Angle -> Double
radians (Radians a) = a

degrees :: Angle -> Double
degrees (Radians a) = a * 180.0 / pi

angleLift :: (Double -> Double) -> Angle -> Angle
angleLift f = Radians . f . radians

angleLift2 :: (Double -> Double -> Double) -> Angle -> Angle -> Angle
angleLift2 f a b = Radians $ f (radians a) (radians b)

angleCos :: Angle -> Double
angleCos = cos . radians

angleSin :: Angle -> Double
angleSin = sin . radians

--instance Show Angle where
--    show ang@(Rads a) = "Rads (deg = " ++ (show (degrees ang)) ++ ")"

instance Num Angle where
  (+) = angleLift2 (+)
  (-) = angleLift2 (-)
  (*) = angleLift2 (*)
  negate = angleLift negate
  abs = angleLift abs
  signum = angleLift signum
  fromInteger = Radians . fromInteger

rayApply :: Ray -> Double -> Point
rayApply (Ray (Point x0 y0) angle) s = Point x y
    where x = s * (angleCos angle) + x0
          y = s * (angleSin angle) + y0

