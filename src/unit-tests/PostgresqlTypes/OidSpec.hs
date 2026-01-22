module PostgresqlTypes.OidSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Word
import qualified PostgresqlTypes.Oid as Oid
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Oid.Oid)

  describe "Constructors" do
    describe "fromWord32" do
      it "creates Oid from Word32" do
        let pgOid = Oid.fromWord32 42
        Oid.toWord32 pgOid `shouldBe` 42

      it "creates Oid from zero" do
        let pgOid = Oid.fromWord32 0
        Oid.toWord32 pgOid `shouldBe` 0

      it "creates Oid from maximum Word32" do
        let pgOid = Oid.fromWord32 4294967295
        Oid.toWord32 pgOid `shouldBe` 4294967295

  describe "Accessors" do
    describe "toWord32" do
      it "extracts Word32 value" do
        let pgOid = Oid.fromWord32 12345
        Oid.toWord32 pgOid `shouldBe` 12345

  describe "Property Tests" do
    it "roundtrips through toWord32 and fromWord32" do
      property \(w :: Word32) ->
        let pgOid = Oid.fromWord32 w
         in Oid.toWord32 pgOid === w

    it "roundtrips through fromWord32 and toWord32" do
      property \(pgOid :: Oid.Oid) ->
        let w = Oid.toWord32 pgOid
            pgOid' = Oid.fromWord32 w
         in pgOid' === pgOid
