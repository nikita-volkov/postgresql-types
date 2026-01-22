module PostgresqlTypes.JsonSpec (spec) where

import qualified Data.Aeson as Aeson
import Data.Data (Proxy (Proxy))
import qualified Data.Vector as Vector
import qualified PostgresqlTypes.Json as Json
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Json.Json)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Json.Json)

  describe "Constructors" do
    describe "fromValue" do
      it "creates Json from Aeson Value (Null)" do
        let jsonVal = Json.normalizeFromAesonValue Aeson.Null
        Json.toAesonValue jsonVal `shouldBe` Aeson.Null

      it "creates Json from Aeson Value (Bool)" do
        let jsonVal = Json.normalizeFromAesonValue (Aeson.Bool True)
        Json.toAesonValue jsonVal `shouldBe` Aeson.Bool True

      it "creates Json from Aeson Value (Number)" do
        let jsonVal = Json.normalizeFromAesonValue (Aeson.Number 42)
        Json.toAesonValue jsonVal `shouldBe` Aeson.Number 42

      it "creates Json from Aeson Value (String)" do
        let jsonVal = Json.normalizeFromAesonValue (Aeson.String "hello")
        Json.toAesonValue jsonVal `shouldBe` Aeson.String "hello"

      it "creates Json from Aeson Value (Array)" do
        let value = Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 2])
            jsonVal = Json.normalizeFromAesonValue value
        Json.toAesonValue jsonVal `shouldBe` value

      it "creates Json from Aeson Value (Object)" do
        let value = Aeson.object ["key" Aeson..= ("value" :: String)]
            jsonVal = Json.normalizeFromAesonValue value
        Json.toAesonValue jsonVal `shouldBe` value

  describe "Accessors" do
    describe "toValue" do
      it "extracts Aeson Value" do
        let value = Aeson.object ["test" Aeson..= (123 :: Int)]
            jsonVal = Json.normalizeFromAesonValue value
        Json.toAesonValue jsonVal `shouldBe` value

  describe "Property Tests" do
    it "roundtrips through fromValue and toValue" do
      property \(jsonVal :: Json.Json) ->
        let value = Json.toAesonValue jsonVal
            jsonVal' = Json.normalizeFromAesonValue value
         in jsonVal' === jsonVal
