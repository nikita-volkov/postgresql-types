module PostgresqlTypes.CircleSpec (spec) where

import Data.Maybe
import qualified PostgresqlTypes.Circle as Circle
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Circle" do
    describe "Constructors" do
      describe "normalizeFromCenterAndRadius" do
        it "creates Circle from center and radius (3-tuple)" do
          let circle = Circle.normalizeFromCenterAndRadius (3.0, 4.0, 5.0)
              (x, y, r) = Circle.toCenterAndRadius circle
          (x, y, r) `shouldBe` (3.0, 4.0, 5.0)

        it "takes absolute value of negative radius" do
          let circle = Circle.normalizeFromCenterAndRadius (0.0, 0.0, (-5.0))
              (_, _, r) = Circle.toCenterAndRadius circle
          r `shouldBe` 5.0

      describe "refineFromCenterAndRadius" do
        it "rejects negative radius" do
          Circle.refineFromCenterAndRadius (0.0, 0.0, (-1.0)) `shouldBe` Nothing

        it "accepts zero radius" do
          let result = Circle.refineFromCenterAndRadius (0.0, 0.0, 0.0)
          result `shouldSatisfy` isJust

        it "accepts positive radius" do
          let result = Circle.refineFromCenterAndRadius (1.0, 2.0, 3.0)
          result `shouldSatisfy` isJust

    describe "Accessors" do
      describe "toCenterAndRadius" do
        it "extracts center and radius as 3-tuple" do
          let circle = Circle.normalizeFromCenterAndRadius (2.5, 3.5, 4.5)
              (x, y, r) = Circle.toCenterAndRadius circle
          (x, y, r) `shouldBe` (2.5, 3.5, 4.5)

    describe "Property Tests" do
      it "roundtrips through toCenterAndRadius and normalizeFromCenterAndRadius" do
        property \(circle :: Circle.Circle) ->
          let centerRadius = Circle.toCenterAndRadius circle
              circle' = Circle.normalizeFromCenterAndRadius centerRadius
           in circle' === circle

      it "normalizeFromCenterAndRadius ensures non-negative radius" do
        property \(x :: Double, y :: Double, r :: Double) ->
          let circle = Circle.normalizeFromCenterAndRadius (x, y, r)
              (_, _, r') = Circle.toCenterAndRadius circle
           in r' >= 0
