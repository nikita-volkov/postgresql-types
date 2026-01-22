module PostgresqlTypes.Int8Spec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Int
import qualified PostgresqlTypes.Int8 as Int8
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Int8.Int8)

  describe "Constructors" do
    describe "fromInt64" do
      it "creates Int8 from positive Int64" do
        let pgInt = Int8.fromInt64 42
        Int8.toInt64 pgInt `shouldBe` 42

      it "creates Int8 from negative Int64" do
        let pgInt = Int8.fromInt64 (-42)
        Int8.toInt64 pgInt `shouldBe` (-42)

      it "creates Int8 from minimum Int64" do
        let pgInt = Int8.fromInt64 (-9223372036854775808)
        Int8.toInt64 pgInt `shouldBe` (-9223372036854775808)

      it "creates Int8 from maximum Int64" do
        let pgInt = Int8.fromInt64 9223372036854775807
        Int8.toInt64 pgInt `shouldBe` 9223372036854775807

  describe "Accessors" do
    describe "toInt64" do
      it "extracts Int64 value" do
        let pgInt = Int8.fromInt64 123456789
        Int8.toInt64 pgInt `shouldBe` 123456789

  describe "Property Tests" do
    it "roundtrips through toInt64 and fromInt64" do
      property \(i :: Int64) ->
        let pgInt = Int8.fromInt64 i
         in Int8.toInt64 pgInt === i

    it "roundtrips through fromInt64 and toInt64" do
      property \(pgInt :: Int8.Int8) ->
        let i = Int8.toInt64 pgInt
            pgInt' = Int8.fromInt64 i
         in pgInt' === pgInt
