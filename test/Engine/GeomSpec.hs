module Engine.GeomSpec (spec) where

import Test.Hspec
import TestHelpers

import Engine.Geom

infix 1 `ptShouldBeAbout`
ptShouldBeAbout :: Point -> Point -> Expectation
ptShouldBeAbout (Point (x,y)) (Point (ex,ey)) = do
    x `shouldBeAbout` ex
    y `shouldBeAbout` ey

{-# ANN spec "HLint: ignore Redundant do" #-}
{-# ANN spec "HLint: ignore Redundant $" #-}
spec :: Spec
spec = do
    describe "Angle" $ do

        describe "creation and access" $ do

            it "should create from radians" $
                radians (Angle 3.14) `shouldBe` 3.14

            it "should create from degrees" $
                radians (fromDegrees 180) `shouldBe` pi

            it "should access as degrees" $
                degrees (Angle pi) `shouldBe` 180.0


    describe "Point" $ do

        describe "creation and accessors" $ do

            it "should create and gets x" $
                ptX (Point (3,4)) `shouldBe` 3

            it "should create and gets y" $
                ptY (Point (3,4)) `shouldBe` 4

            it "should create from a scalar" $
                ptFromDouble 3.2 `shouldBe` Point (3.2, 3.2)


    describe "Line" $ do

        describe "parameterization and intersection" $ do

            -- parameterize line

            it "should be parameterized" $ let
                line = Line (Point (3,4)) (Angle (pi/6))
                result = parameterized line 2.0
                expected = Point (3 + sqrt 3, 4+1)
                in
                  result `ptShouldBeAbout` expected

            -- is intersecting

            it "should show two intersecting lines as intersecting" $ let
                line1 = Line (Point (-1, 10)) (Angle (pi/3))
                line2 = Line (Point (1, 10)) (Angle (pi - pi/3))
                in
                    (line1 `isIntersecting` line2) `shouldBe` True

            it "should show two non-intersecting lines as non-intersecting" $ let
                line1 = Line (Point (-1, 10)) (Angle (pi/4))
                line2 = Line (Point (1, 3)) (Angle (pi/4))
                in
                    (line1 `isIntersecting` line2) `shouldBe` False

            it "should show two overlapping lines as non-intersecting" $ let
                line1 = Line (Point (-1, -1)) (Angle (pi/4))
                line2 = Line (Point (1, 1)) (Angle (pi/4))
                in
                    (line1 `isIntersecting` line2) `shouldBe` False

            -- intersection parameters

            it "should solve the parameters for the intersection of two lines" $ let
                line1 = Line (Point (-1, 10)) (Angle (pi/3))
                line2 = Line (Point (1, 10)) (Angle (pi - pi/3))
                (u, v) = intersectionParameters line1 line2
                point1 = parameterized line1 u
                point2 = parameterized line2 v
                in do
                    u `shouldBeAbout` 2.0
                    v `shouldBeAbout` 2.0
                    point1 `ptShouldBeAbout` point2

            it "should give infinite values for the intersection parameters when the lines overlap at every point" $ let
                line1 = Line (Point (0, 0)) (Angle (pi/4))
                line2 = Line (Point (1, 1)) (Angle (pi/4))
                (u, v) = intersectionParameters line1 line2
                in do
                   u `shouldSatisfy` isInfinite
                   v `shouldSatisfy` isInfinite

            it "should give infinite values for the intersection parameters when the lines never intersect" $ let
                line1 = Line (Point (-1, 10)) (Angle (pi/4))
                line2 = Line (Point (1, 1)) (Angle (pi/4))
                (u, v) = intersectionParameters line1 line2
                in do
                   u `shouldSatisfy` isInfinite
                   v `shouldSatisfy` isInfinite

            -- intersection point

            it "should calculate the intersection of two lines" $ let
                line1 = Line (Point (-1, 10)) (Angle (pi/3))
                line2 = Line (Point (1, 10)) (Angle (pi - pi/3))
                p = intersection line1 line2
                in
                    p `ptShouldBeAbout` Point (0, 10 + sqrt 3)

            it "should calculate the intersection of lines perpendicular to the coordinate axes" $ let
                line1 = Line (Point (1,2)) (Angle 0) -- y = 2 forall x
                line2 = Line (Point (3,4)) (Angle (pi/2)) -- x = 3 forall y
                p = intersection line1 line2
                in
                    p `ptShouldBeAbout` Point (3,2)

            it "should treat like non-intersecting if perfectly overlapped" $ let
                line1 = Line (Point(1,1)) (Angle pi/4)
                line2 = Line (Point(2,2)) (Angle pi/4)
                p = intersection line1 line2
                in do
                    ptX p `shouldSatisfy` isInfinite
                    ptY p `shouldSatisfy` isInfinite

            it "should give +/- Infinity in at least one coordinate if the lines do not intersect at all" $ let
                line1 = Line (Point (0, 0)) (Angle (pi/4))
                line2 = Line (Point (-10, 3)) (Angle (pi/4))
                p = intersection line1 line2
                in do
                    ptX p `shouldSatisfy` isInfinite
                    ptY p `shouldSatisfy` isInfinite