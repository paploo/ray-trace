module Engine.GeomSpec (spec) where

import Test.Hspec
import TestHelpers

import Engine.Geom

infix 1 `ptShouldBeAbout`
ptShouldBeAbout :: Point -> Point -> Expectation
ptShouldBeAbout (Point (x,y)) (Point (ex,ey)) = do
    x `shouldBeAbout` ex
    y `shouldBeAbout` ey


spec :: Spec
spec = do
    describe "Angle" $ do

        describe "creation and access" $ do

            it "should create from radians" $ do
                (radians (Angle 3.14)) `shouldBe` 3.14

            it "should create from degrees" $ do
                (radians (fromDegrees 180)) `shouldBe` pi

            it "should access as degrees" $ do
                (degrees (Angle pi)) `shouldBe` 180.0


    describe "Point" $ do

        describe "creation and accessors" $ do

            it "should create and gets x" $ do
                (ptX (Point (3,4))) `shouldBe` 3

            it "should create and gets y" $ do
                (ptY (Point (3,4))) `shouldBe` 4

            it "should create from a scalar" $ do
                (ptFromDouble 3.2) `shouldBe` (Point (3.2, 3.2))


    describe "Line" $ do

        describe "parameterization and intersection" $ do

            it "should solve for the intersection of two rays" $ let
                line1 = Line (Point (-1, 10)) (Angle (pi/3))
                line2 = Line (Point (1, 10)) (Angle (pi - pi/3))
                (u, v) = intersectionParameters line1 line2
                point1 = parameterized line1 $ u
                point2 = parameterized line2 $ v
                in do
                    point1 `ptShouldBeAbout` point2


            it "should be parameterized" $ let
                line = Line (Point (3,4)) (Angle (pi/6))
                result = ((parameterized line) 2.0)
                expected = Point ((3 + sqrt 3), (4+1))
                in do
                  result `ptShouldBeAbout` expected
