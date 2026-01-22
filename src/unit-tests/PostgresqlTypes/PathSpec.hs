module PostgresqlTypes.PathSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified PostgresqlTypes.Path as Path
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Path.Path)

  describe "Constructors" do
    describe "refineFromPointList (open path)" do
      it "creates open Path from list of points" do
        let points = [(0.0, 0.0), (1.0, 1.0), (2.0, 2.0)]
            result = Path.refineFromPointList (False, points)
        result `shouldSatisfy` isJust
        fmap Path.toPointList result `shouldBe` Just (False, points)

    describe "refineFromPointList (closed path)" do
      it "creates closed Path from list of points" do
        let points = [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0)]
            result = Path.refineFromPointList (True, points)
        result `shouldSatisfy` isJust
        fmap Path.toPointList result `shouldBe` Just (True, points)

  describe "Accessors" do
    describe "toPointList" do
      it "extracts closed flag and points" do
        let points = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]
        path <- case Path.refineFromPointList (False, points) of
          Just p -> return p
          Nothing -> error "Failed to create Path"
        let (isClosed, extractedPoints) = Path.toPointList path
        isClosed `shouldBe` False
        extractedPoints `shouldBe` points

  describe "Property Tests" do
    it "roundtrips through toPointList and refineFromPointList" do
      property \(path :: Path.Path) ->
        let pointsData = Path.toPointList path
            result = Path.refineFromPointList pointsData
         in result === Just path
