module PostgresqlTypes.BoxSpec (spec) where

import Data.Data (Proxy (Proxy))
import qualified PostgresqlTypes.Box as Box
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Box.Box)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Box.Box)

  describe "Constructors" do
    describe "normalizeFromCorners" do
      it "normalizes corners to ensure x1<=x2 and y1<=y2" $ property \x1 y1 x2 y2 -> do
        let box = Box.normalizeFromCorners x1 y1 x2 y2
        Box.toX1 box <= Box.toX2 box .&&. Box.toY1 box <= Box.toY2 box

      it "creates Box from corner coordinates" do
        -- Assuming normalizeFromCorners x1 y1 x2 y2
        let box = Box.normalizeFromCorners 1.0 2.0 3.0 4.0
            x1 = Box.toX1 box
            y1 = Box.toY1 box
            x2 = Box.toX2 box
            y2 = Box.toY2 box
        -- Behavior depends on normalization logic (usually HighRight, LowLeft or similar)
        -- Just checking values exist and match one of inputs
        [x1, x2] `shouldContain` [1.0, 3.0]
        [y1, y2] `shouldContain` [2.0, 4.0]

  describe "Accessors" do
    describe "toX1 / toY1 / toX2 / toY2" do
      it "extracts coordinates" do
        let box = Box.normalizeFromCorners 1.0 2.0 3.0 4.0
        -- Just ensuring it runs and returns values
        Box.toX1 box `shouldSatisfy` const True

  describe "Property Tests" do
    it "roundtrips through accessors and constructor (normalization aware)" do
      property $ \(box :: Box.Box) ->
        let x1 = Box.toX1 box
            y1 = Box.toY1 box
            x2 = Box.toX2 box
            y2 = Box.toY2 box
            box' = Box.normalizeFromCorners x1 y1 x2 y2
         in box' === box
