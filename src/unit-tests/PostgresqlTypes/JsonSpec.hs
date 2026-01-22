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
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Json.Json)

  describe "Constructors" do
    describe "fromValue" do
      it "creates Json from Aeson Value (Null)" do
        let jsonVal = Json.fromValue Aeson.Null
        Json.toValue jsonVal `shouldBe` Aeson.Null

      it "creates Json from Aeson Value (Bool)" do
        let jsonVal = Json.fromValue (Aeson.Bool True)
        Json.toValue jsonVal `shouldBe` Aeson.Bool True

      it "creates Json from Aeson Value (Number)" do
        let jsonVal = Json.fromValue (Aeson.Number 42)
        Json.toValue jsonVal `shouldBe` Aeson.Number 42

      it "creates Json from Aeson Value (String)" do
        let jsonVal = Json.fromValue (Aeson.String "hello")
        Json.toValue jsonVal `shouldBe` Aeson.String "hello"

      it "creates Json from Aeson Value (Array)" do
        let value = Aeson.Array (Vector.fromList [Aeson.Number 1, Aeson.Number 2])
            jsonVal = Json.fromValue value
        Json.toValue jsonVal `shouldBe` value

      it "creates Json from Aeson Value (Object)" do
        let value = Aeson.object ["key" Aeson..= ("value" :: String)]
            jsonVal = Json.fromValue value
        Json.toValue jsonVal `shouldBe` value

  describe "Accessors" do
    describe "toValue" do
      it "extracts Aeson Value" do
        let value = Aeson.object ["test" Aeson..= (123 :: Int)]
            jsonVal = Json.fromValue value
        Json.toValue jsonVal `shouldBe` value

  describe "Property Tests" do
    it "roundtrips through fromValue and toValue" do
      property \(jsonVal :: Json.Json) ->
        let value = Json.toValue jsonVal
            jsonVal' = Json.fromValue value
         in jsonVal' === jsonVal
