module Int4Spec (spec) where

import Data.Int
import qualified PostgresqlTypes.Int4 as Int4
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Int4" do
    describe "Constructors" do
      describe "fromInt32" do
        it "creates Int4 from positive Int32" do
          let pgInt = Int4.fromInt32 42
          Int4.toInt32 pgInt `shouldBe` 42

        it "creates Int4 from negative Int32" do
          let pgInt = Int4.fromInt32 (-42)
          Int4.toInt32 pgInt `shouldBe` (-42)

        it "creates Int4 from minimum Int32" do
          let pgInt = Int4.fromInt32 (-2147483648)
          Int4.toInt32 pgInt `shouldBe` (-2147483648)

        it "creates Int4 from maximum Int32" do
          let pgInt = Int4.fromInt32 2147483647
          Int4.toInt32 pgInt `shouldBe` 2147483647

    describe "Accessors" do
      describe "toInt32" do
        it "extracts Int32 value" do
          let pgInt = Int4.fromInt32 12345
          Int4.toInt32 pgInt `shouldBe` 12345

    describe "Property Tests" do
      it "roundtrips through toInt32 and fromInt32" do
        property \(i :: Int32) ->
          let pgInt = Int4.fromInt32 i
           in Int4.toInt32 pgInt === i

      it "roundtrips through fromInt32 and toInt32" do
        property \(pgInt :: Int4.Int4) ->
          let i = Int4.toInt32 pgInt
              pgInt' = Int4.fromInt32 i
           in pgInt' === pgInt
