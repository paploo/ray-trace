module AngleSpec (spec) where

import Test.Hspec
import Angle

spec :: Spec
spec = do
    describe "basic creation and access" $ do

        it "creates from radians" $
            (radians (Angle 3.14)) `shouldBe` 3.14

        it "creates from degrees" $
            (radians (fromDegrees 180)) `shouldBe` pi
