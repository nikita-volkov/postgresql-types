module JsonbSpec (spec) where

import qualified Data.Aeson as Aeson
import Data.Maybe
import qualified Data.Vector as Vector
import qualified PostgresqlTypes.Jsonb as Jsonb
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Jsonb" do
    describe "Constructors" do
      describe "normalizeFromValue" do
        it "creates Jsonb from Aeson Value (Null)" do
          let jsonb = Jsonb.normalizeFromValue Aeson.Null
          Jsonb.toValue jsonb `shouldBe` Aeson.Null

        it "creates Jsonb from Aeson Value (Bool)" do
          let jsonb = Jsonb.normalizeFromValue (Aeson.Bool True)
          Jsonb.toValue jsonb `shouldBe` Aeson.Bool True

        it "creates Jsonb from Aeson Value (Number)" do
          let jsonb = Jsonb.normalizeFromValue (Aeson.Number 42)
          Jsonb.toValue jsonb `shouldBe` Aeson.Number 42

      describe "refineFromValue" do
        it "accepts valid Aeson Value" do
          let result = Jsonb.refineFromValue Aeson.Null
          result `shouldSatisfy` isJust

    describe "Accessors" do
      describe "toValue" do
        it "extracts Aeson Value" do
          let value = Aeson.object ["test" Aeson..= (123 :: Int)]
              jsonb = Jsonb.normalizeFromValue value
          Jsonb.toValue jsonb `shouldBe` value

    describe "Property Tests" do
      it "roundtrips through normalizeFromValue and toValue" do
        property \(jsonb :: Jsonb.Jsonb) ->
          let value = Jsonb.toValue jsonb
              jsonb' = Jsonb.normalizeFromValue value
           in jsonb' === jsonb
