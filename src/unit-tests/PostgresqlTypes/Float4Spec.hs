module PostgresqlTypes.Float4Spec (spec) where

import qualified PostgresqlTypes.Float4 as Float4
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Float4" do
    describe "Constructors" do
      describe "fromFloat" do
        it "creates Float4 from positive Float" do
          let pgFloat = Float4.fromFloat 3.14
          Float4.toFloat pgFloat `shouldBe` 3.14

        it "creates Float4 from negative Float" do
          let pgFloat = Float4.fromFloat (-3.14)
          Float4.toFloat pgFloat `shouldBe` (-3.14)

        it "creates Float4 from zero" do
          let pgFloat = Float4.fromFloat 0
          Float4.toFloat pgFloat `shouldBe` 0

    describe "Accessors" do
      describe "toFloat" do
        it "extracts Float value" do
          let pgFloat = Float4.fromFloat 1.5
          Float4.toFloat pgFloat `shouldBe` 1.5

    describe "Property Tests" do
      it "roundtrips through toFloat and fromFloat" do
        property \(f :: Float) ->
          let pgFloat = Float4.fromFloat f
           in Float4.toFloat pgFloat === f

      it "roundtrips through fromFloat and toFloat" do
        property \(pgFloat :: Float4.Float4) ->
          let f = Float4.toFloat pgFloat
              pgFloat' = Float4.fromFloat f
           in pgFloat' === pgFloat
