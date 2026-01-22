module PostgresqlTypes.PointSpec (spec) where

import Data.Data (Proxy (Proxy))
import qualified PostgresqlTypes.Point as Point
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Point.Point)

  describe "Constructors" do
    describe "fromCoordinates" do
      it "creates Point from coordinates" do
        let point = Point.fromCoordinates 3.5 4.2
            x = Point.toX point
            y = Point.toY point
        x `shouldBe` 3.5
        y `shouldBe` 4.2

      it "creates Point at origin" do
        let point = Point.fromCoordinates 0 0
            x = Point.toX point
            y = Point.toY point
        x `shouldBe` 0
        y `shouldBe` 0

      it "creates Point with negative coordinates" do
        let point = Point.fromCoordinates (-1.5) (-2.3)
            x = Point.toX point
            y = Point.toY point
        x `shouldBe` (-1.5)
        y `shouldBe` (-2.3)

  describe "Accessors" do
    describe "toX and toY" do
      it "extract individual coordinates" do
        let point = Point.fromCoordinates 10.5 20.7
            x = Point.toX point
            y = Point.toY point
        (x, y) `shouldBe` (10.5, 20.7)

  describe "Property Tests" do
    it "roundtrips through toX/toY and fromCoordinates" do
      property \(x :: Double, y :: Double) ->
        let point = Point.fromCoordinates x y
            x' = Point.toX point
            y' = Point.toY point
         in (x', y') === (x, y)

    it "roundtrips through fromCoordinates and toX/toY" do
      property \(point :: Point.Point) ->
        let x = Point.toX point
            y = Point.toY point
            point' = Point.fromCoordinates x y
         in point' === point
