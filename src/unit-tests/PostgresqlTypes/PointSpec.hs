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
        let point = Point.fromCoordinates (3.5, 4.2)
            (x, y) = Point.toCoordinates point
        x `shouldBe` 3.5
        y `shouldBe` 4.2

      it "creates Point at origin" do
        let point = Point.fromCoordinates (0, 0)
            (x, y) = Point.toCoordinates point
        x `shouldBe` 0
        y `shouldBe` 0

      it "creates Point with negative coordinates" do
        let point = Point.fromCoordinates (-1.5, -2.3)
            (x, y) = Point.toCoordinates point
        x `shouldBe` (-1.5)
        y `shouldBe` (-2.3)

  describe "Accessors" do
    describe "toCoordinates" do
      it "extracts coordinates as tuple" do
        let point = Point.fromCoordinates (10.5, 20.7)
            (x, y) = Point.toCoordinates point
        (x, y) `shouldBe` (10.5, 20.7)

  describe "Property Tests" do
    it "roundtrips through toCoordinates and fromCoordinates" do
      property \(x :: Double, y :: Double) ->
        let point = Point.fromCoordinates (x, y)
            (x', y') = Point.toCoordinates point
         in (x', y') === (x, y)

    it "roundtrips through fromCoordinates and toCoordinates" do
      property \(point :: Point.Point) ->
        let coords = Point.toCoordinates point
            point' = Point.fromCoordinates coords
         in point' === point
