module PostgresqlTypes.BoolSpec (spec) where

import qualified Data.Bool
import Data.Data (Proxy (Proxy))
import qualified PostgresqlTypes.Bool as Bool
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Bool.Bool)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Bool.Bool)

  describe "Constructors" do
    describe "fromBool" do
      it "creates Bool from True" do
        let pgBool = Bool.fromBool True
        Bool.toBool pgBool `shouldBe` True

      it "creates Bool from False" do
        let pgBool = Bool.fromBool False
        Bool.toBool pgBool `shouldBe` False

  describe "Accessors" do
    describe "toBool" do
      it "extracts True correctly" do
        let pgBool = Bool.fromBool True
        Bool.toBool pgBool `shouldBe` True

      it "extracts False correctly" do
        let pgBool = Bool.fromBool False
        Bool.toBool pgBool `shouldBe` False

  describe "Property Tests" do
    it "roundtrips through toBool and fromBool" do
      property \(b :: Data.Bool.Bool) ->
        let pgBool = Bool.fromBool b
         in Bool.toBool pgBool === b

    it "roundtrips through fromBool and toBool" do
      property \(pgBool :: Bool.Bool) ->
        let b = Bool.toBool pgBool
            pgBool' = Bool.fromBool b
         in pgBool' === pgBool
