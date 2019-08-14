module Engine.AngleSpec (spec) where

import Test.Hspec
import Engine.Angle

spec :: Spec
spec = do
    describe "basic creation and access" $ do

        it "creates from radians" $
            (radians (Angle 3.14)) `shouldBe` 3.14

        it "creates from degrees" $
            (radians (fromDegrees 180)) `shouldBe` pi

        it "access degrees" $
            (degrees (Angle pi)) `shouldBe` 180.0
