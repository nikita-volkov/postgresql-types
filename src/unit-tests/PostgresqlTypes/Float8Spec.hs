module PostgresqlTypes.Float8Spec (spec) where

import qualified PostgresqlTypes.Float8 as Float8
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Float8" do
    describe "Constructors" do
      describe "fromDouble" do
        it "creates Float8 from positive Double" do
          let pgFloat = Float8.fromDouble 3.14159265358979
          Float8.toDouble pgFloat `shouldBe` 3.14159265358979

        it "creates Float8 from negative Double" do
          let pgFloat = Float8.fromDouble (-3.14159265358979)
          Float8.toDouble pgFloat `shouldBe` (-3.14159265358979)

        it "creates Float8 from zero" do
          let pgFloat = Float8.fromDouble 0
          Float8.toDouble pgFloat `shouldBe` 0

    describe "Accessors" do
      describe "toDouble" do
        it "extracts Double value" do
          let pgFloat = Float8.fromDouble 1.5
          Float8.toDouble pgFloat `shouldBe` 1.5

    describe "Property Tests" do
      it "roundtrips through toDouble and fromDouble" do
        property \(d :: Double) ->
          let pgFloat = Float8.fromDouble d
           in Float8.toDouble pgFloat === d

      it "roundtrips through fromDouble and toDouble" do
        property \(pgFloat :: Float8.Float8) ->
          let d = Float8.toDouble pgFloat
              pgFloat' = Float8.fromDouble d
           in pgFloat' === pgFloat
