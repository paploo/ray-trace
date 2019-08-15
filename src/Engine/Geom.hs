{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Engine.Geom (
    Angle(Angle),
    radians,
    zeroAng,
    fromDegrees,
    degrees,
    reversed,
    unitCirclePt,

    Point(Point),
    toTuple,
    ptX,
    ptY,
    zeroPt,
    ptFromDouble,
    scalePt,
    unitCircleAng,

    Line(Line),
    parameterized,
    intersectionParameters
) where

---
--- Angle
---

newtype Angle = Angle { radians :: Double } deriving (Show, Eq, Ord, Num, Fractional)

zeroAng :: Angle
zeroAng = 0.0

fromDegrees :: Double -> Angle
fromDegrees a = Angle (a * pi / 180.0)

degrees :: Angle -> Double
degrees (Angle a) = a * 180.0 / pi

reversed :: Angle -> Angle
reversed (Angle a) = Angle (a - 180.0)

unitCirclePt :: Angle -> Point
unitCirclePt (Angle a) = Point ((cos a), (sin a))

instance Semigroup Angle where
    (<>) = (+)

instance Monoid Angle where
    mempty = zeroAng

--
-- Point
--

newtype Point = Point { toTuple :: (Double, Double) } deriving (Eq)

ptX :: Point -> Double
ptX (Point (x,_)) = x

ptY :: Point -> Double
ptY (Point (_,y)) = y

zeroPt :: Point
zeroPt = Point (0.0, 0.0)

ptFromDouble :: Double -> Point
ptFromDouble a = Point (a, a)

scalePt :: Point -> Double -> Point
scalePt p = (* p) . ptFromDouble

unitCircleAng :: Point -> Angle
unitCircleAng (Point (x,y)) = Angle (atan2 y x)

ptMap :: (Double -> Double) -> Point -> Point
ptMap f (Point (x,y)) = Point (f x, f y)

ptMap2 :: (Double -> Double -> Double) -> Point -> Point -> Point
ptMap2 f (Point (x1, y1)) (Point (x2, y2)) = Point (f x1 x2, f y1 y2)

instance Show Point where
    show (Point (x,y)) = "Point (" ++ (show x) ++ ", " ++ (show y) ++ ")"

instance Num Point where
    (+) = ptMap2 (+)
    (-) = ptMap2 (-)
    (*) = ptMap2 (*)
    negate = ptMap negate
    abs = ptMap abs
    signum = ptMap signum
    fromInteger k = Point (kd, kd) where kd = fromInteger k

instance Fractional Point where
    (/) = ptMap2 (/)
    fromRational r = Point (rd, rd) where rd = fromRational r

instance Semigroup Point where
    (<>) = (+)

instance Monoid Point where
    mempty = zeroPt

---
--- Line
---

data Line = Line {
    linePt :: Point,
    lineAng :: Angle
} deriving (Show, Eq)

parameterized :: Line -> Double -> Point
parameterized (Line p a) t = (unitCirclePt a) * (ptFromDouble t) + p

intersectionParameters :: Line -> Line -> (Double, Double)
intersectionParameters (Line (Point (x1,y1)) (Angle a1)) (Line (Point (x2,y2)) (Angle a2)) = (u,v)
    where
        dx = x2 - x1
        dy = y2 - y1
        da = a2 - a1
        csc = 1.0 / (sin da)
        u = csc * (dx * sin a2 - dy * cos a2)
        v = csc * (dx * sin a1 - dy * cos a1)
