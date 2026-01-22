module PostgresqlTypes.JsonbSpec (spec) where

import qualified Data.Aeson as Aeson
import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified PostgresqlTypes.Jsonb as Jsonb
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Jsonb.Jsonb)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Jsonb.Jsonb)

  describe "Constructors" do
    describe "normalizeFromValue" do
      it "creates Jsonb from Aeson Value (Null)" do
        let jsonb = Jsonb.normalizeFromAesonValue Aeson.Null
        Jsonb.toAesonValue jsonb `shouldBe` Aeson.Null

      it "creates Jsonb from Aeson Value (Bool)" do
        let jsonb = Jsonb.normalizeFromAesonValue (Aeson.Bool True)
        Jsonb.toAesonValue jsonb `shouldBe` Aeson.Bool True

      it "creates Jsonb from Aeson Value (Number)" do
        let jsonb = Jsonb.normalizeFromAesonValue (Aeson.Number 42)
        Jsonb.toAesonValue jsonb `shouldBe` Aeson.Number 42

    describe "refineFromValue" do
      it "accepts valid Aeson Value" do
        let result = Jsonb.refineFromAesonValue Aeson.Null
        result `shouldSatisfy` isJust

  describe "Accessors" do
    describe "toValue" do
      it "extracts Aeson Value" do
        let value = Aeson.object ["test" Aeson..= (123 :: Int)]
            jsonb = Jsonb.normalizeFromAesonValue value
        Jsonb.toAesonValue jsonb `shouldBe` value

  describe "Property Tests" do
    it "roundtrips through normalizeFromValue and toValue" do
      property \(jsonb :: Jsonb.Jsonb) ->
        let value = Jsonb.toAesonValue jsonb
            jsonb' = Jsonb.normalizeFromAesonValue value
         in jsonb' === jsonb
