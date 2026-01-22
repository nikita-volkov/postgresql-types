module PostgresqlTypes.CircleSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified PostgresqlTypes.Circle as Circle
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Circle.Circle)

  describe "Constructors" do
    describe "normalizeFromCenterAndRadius" do
      it "creates Circle from center and radius" do
        let circle = Circle.normalizeFromCenterAndRadius 3.0 4.0 5.0
            x = Circle.toCenterX circle
            y = Circle.toCenterY circle
            r = Circle.toRadius circle
        (x, y, r) `shouldBe` (3.0, 4.0, 5.0)

      it "takes absolute value of negative radius" do
        let circle = Circle.normalizeFromCenterAndRadius 0.0 0.0 (-5.0)
            r = Circle.toRadius circle
        r `shouldBe` 5.0

    describe "refineFromCenterAndRadius" do
      it "rejects negative radius" do
        Circle.refineFromCenterAndRadius 0.0 0.0 (-1.0) `shouldBe` Nothing

      it "accepts zero radius" do
        let result = Circle.refineFromCenterAndRadius 0.0 0.0 0.0
        result `shouldSatisfy` isJust

      it "accepts positive radius" do
        let result = Circle.refineFromCenterAndRadius 1.0 2.0 3.0
        result `shouldSatisfy` isJust

  describe "Accessors" do
    describe "toCenterX, toCenterY, toRadius" do
      it "extract center coordinates and radius" do
        let circle = Circle.normalizeFromCenterAndRadius 2.5 3.5 4.5
            x = Circle.toCenterX circle
            y = Circle.toCenterY circle
            r = Circle.toRadius circle
        (x, y, r) `shouldBe` (2.5, 3.5, 4.5)

  describe "Property Tests" do
    it "roundtrips through toCenterAndRadius and normalizeFromCenterAndRadius" do
      property \circle ->
        let x = Circle.toCenterX circle
            y = Circle.toCenterY circle
            r = Circle.toRadius circle
            circle' = Circle.normalizeFromCenterAndRadius x y r
         in circle' === circle

    it "normalizeFromCenterAndRadius ensures non-negative radius" do
      property \x y r ->
        let circle = Circle.normalizeFromCenterAndRadius x y r
            r' = Circle.toRadius circle
         in r' >= 0
