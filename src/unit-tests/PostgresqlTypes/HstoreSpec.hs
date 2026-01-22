module PostgresqlTypes.HstoreSpec (spec) where

import Data.Data (Proxy (Proxy))
import qualified Data.Map.Strict as Map
import qualified PostgresqlTypes.Hstore as Hstore
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Hstore.Hstore)

  describe "Constructors" do
    describe "normalizeFromMap" do
      it "creates Hstore from Map" do
        let m = Map.fromList [("key1", Just "value1"), ("key2", Just "value2")]
            hstore = Hstore.normalizeFromMap m
        Hstore.toMap hstore `shouldBe` m

      it "creates Hstore from empty Map" do
        let m = Map.empty
            hstore = Hstore.normalizeFromMap m
        Hstore.toMap hstore `shouldBe` m

      it "handles NULL values" do
        let m = Map.fromList [("key", Nothing)]
            hstore = Hstore.normalizeFromMap m
        Hstore.toMap hstore `shouldBe` m

  describe "Accessors" do
    describe "toMap" do
      it "extracts Map" do
        let m = Map.fromList [("test", Just "data")]
            hstore = Hstore.normalizeFromMap m
        Hstore.toMap hstore `shouldBe` m

  describe "Property Tests" do
    it "roundtrips through toMap and normalizeFromMap" do
      property \(hstore :: Hstore.Hstore) ->
        let m = Hstore.toMap hstore
            hstore' = Hstore.normalizeFromMap m
         in hstore' === hstore

    it "roundtrips through normalizeFromMap and toMap" do
      property \(hstore :: Hstore.Hstore) ->
        let m = Hstore.toMap hstore
            hstore' = Hstore.normalizeFromMap m
         in hstore' === hstore
