{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Engine.Geom (
    Angle(Angle),
    radians,
    fromDegrees,
    degrees,
    reverseAngle,

    Point(Point),
    toTuple,
    ptX,
    ptY,
    pointMap,
    pointMap2
) where

---
--- Angle
---

newtype Angle = Angle { radians :: Double } deriving (Show, Eq, Ord, Num, Fractional)

fromDegrees :: Double -> Angle
fromDegrees a = Angle (a * pi / 180.0)

degrees :: Angle -> Double
degrees (Angle a) = a * 180.0 / pi

reverseAngle :: Angle -> Angle
reverseAngle (Angle a) = Angle (a - 180.0)

unitCircle :: Angle -> Point
unitCircle (Angle a) = Point ((cos a), (sin a))

instance Semigroup Angle where
    (<>) = (+)

instance Monoid Angle where
    mempty = 0.0

--
-- Point
--

newtype Point = Point { toTuple :: (Double, Double) } deriving (Eq)

ptX :: Point -> Double
ptX (Point (x,_)) = x

ptY :: Point -> Double
ptY (Point (_,y)) = y

distance :: Point -> Point -> Double
distance p1 p2 = sqrt (x*x + y*y)
    where Point (x, y) = (p2 - p1)

pointMap :: (Double -> Double) -> Point -> Point
pointMap f (Point (x,y)) = Point (f x, f y)

pointMap2 :: (Double -> Double -> Double) -> Point -> Point -> Point
pointMap2 f (Point (x1, y1)) (Point (x2, y2)) = Point (f x1 x2, f y1 y2)

instance Show Point where
    show (Point (x,y)) = "Point (" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance Num Point where
    (+) = pointMap2 (+)
    (-) = pointMap2 (-)
    (*) = pointMap2 (*)
    negate = pointMap negate
    abs = pointMap abs
    signum = pointMap signum
    fromInteger k = Point (kd, kd) where kd = fromInteger k

instance Fractional Point where
    (/) = pointMap2 (/)
    fromRational r = Point (rd, rd) where rd = fromRational r

instance Semigroup Point where
    (<>) = (+)

instance Monoid Point where
    mempty = Point (0.0, 0.0)

