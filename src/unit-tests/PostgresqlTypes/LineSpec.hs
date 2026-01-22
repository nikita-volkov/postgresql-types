module PostgresqlTypes.LineSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified PostgresqlTypes.Line as Line
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Line.Line)

  describe "Constructors" do
    describe "normalizeFromEquation" do
      it "creates Line from equation coefficients" do
        let line = Line.normalizeFromEquation 1.0 2.0 3.0
            a = Line.toA line
            b = Line.toB line
            c = Line.toC line
        (a, b, c) `shouldBe` (1.0, 2.0, 3.0)

      it "normalizes invalid equations (A=0, B=0)" do
        let line = Line.normalizeFromEquation 0.0 0.0 5.0
            a = Line.toA line
            b = Line.toB line
        -- PostgreSQL normalizes to A=1, B=0, C=0 for invalid lines
        a /= 0 || b /= 0 `shouldBe` True

    describe "refineFromEquation" do
      it "rejects equations where A=0 and B=0" do
        Line.refineFromEquation 0.0 0.0 5.0 `shouldBe` Nothing

      it "accepts valid equations" do
        let result = Line.refineFromEquation 1.0 2.0 3.0
        result `shouldSatisfy` isJust

  describe "Accessors" do
    describe "toA, toB, toC" do
      it "extract equation coefficients" do
        let line = Line.normalizeFromEquation 2.0 (-3.0) 4.0
            a = Line.toA line
            b = Line.toB line
            c = Line.toC line
        (a, b, c) `shouldBe` (2.0, -3.0, 4.0)

  describe "Property Tests" do
    it "roundtrips through toA/toB/toC and normalizeFromEquation" do
      property \(line :: Line.Line) ->
        let a = Line.toA line
            b = Line.toB line
            c = Line.toC line
            line' = Line.normalizeFromEquation a b c
         in line' === line
