module TestHelpers (
  expectTrue
, shouldBeAbout
) where

import Test.Hspec
-- See http://hackage.haskell.org/package/hspec-expectations-0.8.2/docs/src/Test-Hspec-Expectations.html
import Test.Hspec.Expectations (Expectation)

import Control.Monad (unless)

-- Stolen from Test.HSpec.Expectations, but they for some reason didn't expose it to us :eyeroll:
expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

infix 1 `shouldBeAbout`
shouldBeAbout :: Double -> Double -> Expectation
shouldBeAbout value expected = expectTrue errorMsg (delta <= epsilon)
    where
        epsilon = 1e-12
        delta = abs $ expected - value
        errorMsg = show value <> " is not within tolerances of " <> show expected <> " (delta = " <> show delta <> ")"

-- This is all Prelude does for these on `read "Infinity"` and `read "NaN"` (in convert Frac)
--infinity :: Double
--infinity = 1 / 0
--
--nan :: Double
--nan = 0 / 0