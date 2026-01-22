module PolygonSpec (spec) where

import Data.Maybe
import qualified PostgresqlTypes.Polygon as Polygon
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Polygon" do
    describe "Constructors" do
      describe "refineFromPointList" do
        it "creates Polygon from list of points" do
          let points = [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)]
              result = Polygon.refineFromPointList points
          result `shouldSatisfy` isJust
          fmap Polygon.toPointList result `shouldBe` Just points

        it "creates triangle from 3 points" do
          let points = [(0.0, 0.0), (1.0, 0.0), (0.5, 1.0)]
              result = Polygon.refineFromPointList points
          result `shouldSatisfy` isJust

    describe "Accessors" do
      describe "toPointList" do
        it "extracts list of points" do
          let points = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0), (7.0, 8.0)]
              Just polygon = Polygon.refineFromPointList points
          Polygon.toPointList polygon `shouldBe` points

    describe "Property Tests" do
      it "roundtrips through toPointList and refineFromPointList" do
        property \(polygon :: Polygon.Polygon) ->
          let points = Polygon.toPointList polygon
              result = Polygon.refineFromPointList points
           in result === Just polygon
