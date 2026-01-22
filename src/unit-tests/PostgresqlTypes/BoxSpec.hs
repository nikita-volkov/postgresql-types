module PostgresqlTypes.BoxSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified PostgresqlTypes.Box as Box
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Box.Box)

  describe "Constructors" do
    describe "normalizeFromCorners" do
      it "normalizes corners to ensure x1<=x2 and y1<=y2" do
        let box = Box.normalizeFromCorners 3.0 4.0 1.0 2.0
        Box.toX1 box `shouldSatisfy` (<= Box.toX2 box)
        Box.toY1 box `shouldSatisfy` (<= Box.toY2 box)

      it "handles already normalized corners" do
        let box = Box.normalizeFromCorners 1.0 2.0 3.0 4.0
        (Box.toX1 box, Box.toY1 box) `shouldBe` (1.0, 2.0)
        (Box.toX2 box, Box.toY2 box) `shouldBe` (3.0, 4.0)

    describe "refineFromCorners" do
      it "accepts normalized corners" do
        let result = Box.refineFromCorners 1.0 2.0 3.0 4.0
        result `shouldSatisfy` isJust

      it "rejects non-normalized corners" do
        Box.refineFromCorners 3.0 4.0 1.0 2.0 `shouldBe` Nothing

  describe "Accessors" do
    describe "toCorners" do
      it "extracts corners as 4-tuple" do
        let box = Box.normalizeFromCorners 1.0 2.0 5.0 6.0
            corners = Box.toCorners box
        corners `shouldBe` (1.0, 2.0, 5.0, 6.0)

    describe "toX1, toY1, toX2, toY2" do
      it "extract individual coordinates" do
        let box = Box.normalizeFromCorners 1.5 2.5 3.5 4.5
        Box.toX1 box `shouldBe` 1.5
        Box.toY1 box `shouldBe` 2.5
        Box.toX2 box `shouldBe` 3.5
        Box.toY2 box `shouldBe` 4.5

  describe "Property Tests" do
    it "roundtrips through toCorners and normalizeFromCorners" do
      property \(box :: Box.Box) ->
        let (x1, y1, x2, y2) = Box.toCorners box
            box' = Box.normalizeFromCorners x1 y1 x2 y2
         in box' === box

    it "normalizeFromCorners ensures x1<=x2 and y1<=y2" do
      property \(x1 :: Double, y1 :: Double, x2 :: Double, y2 :: Double) ->
        let box = Box.normalizeFromCorners x1 y1 x2 y2
         in Box.toX1 box <= Box.toX2 box && Box.toY1 box <= Box.toY2 box
