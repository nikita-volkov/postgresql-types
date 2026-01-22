module Int2Spec (spec) where

import Data.Int
import qualified PostgresqlTypes.Int2 as Int2
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Int2" do
    describe "Constructors" do
      describe "fromInt16" do
        it "creates Int2 from positive Int16" do
          let pgInt = Int2.fromInt16 42
          Int2.toInt16 pgInt `shouldBe` 42

        it "creates Int2 from negative Int16" do
          let pgInt = Int2.fromInt16 (-42)
          Int2.toInt16 pgInt `shouldBe` (-42)

        it "creates Int2 from minimum Int16" do
          let pgInt = Int2.fromInt16 (-32768)
          Int2.toInt16 pgInt `shouldBe` (-32768)

        it "creates Int2 from maximum Int16" do
          let pgInt = Int2.fromInt16 32767
          Int2.toInt16 pgInt `shouldBe` 32767

    describe "Accessors" do
      describe "toInt16" do
        it "extracts Int16 value" do
          let pgInt = Int2.fromInt16 123
          Int2.toInt16 pgInt `shouldBe` 123

    describe "Property Tests" do
      it "roundtrips through toInt16 and fromInt16" do
        property \(i :: Int16) ->
          let pgInt = Int2.fromInt16 i
           in Int2.toInt16 pgInt === i

      it "roundtrips through fromInt16 and toInt16" do
        property \(pgInt :: Int2.Int2) ->
          let i = Int2.toInt16 pgInt
              pgInt' = Int2.fromInt16 i
           in pgInt' === pgInt
