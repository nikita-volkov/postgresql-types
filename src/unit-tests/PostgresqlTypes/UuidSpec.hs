module PostgresqlTypes.UuidSpec (spec) where

import Data.Data (Proxy (Proxy))
import qualified Data.UUID as UUID
import qualified PostgresqlTypes.Uuid as Uuid
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Uuid.Uuid)

  describe "Constructors" do
    describe "fromUUID" do
      it "creates Uuid from UUID" do
        let uuid = UUID.nil
            pgUuid = Uuid.fromUUID uuid
        Uuid.toUUID pgUuid `shouldBe` uuid

  describe "Accessors" do
    describe "toUUID" do
      it "extracts UUID value" do
        let uuid = UUID.nil
            pgUuid = Uuid.fromUUID uuid
        Uuid.toUUID pgUuid `shouldBe` uuid

  describe "Property Tests" do
    it "roundtrips through toUUID and fromUUID" do
      property \(uuid :: UUID.UUID) ->
        let pgUuid = Uuid.fromUUID uuid
         in Uuid.toUUID pgUuid === uuid

    it "roundtrips through fromUUID and toUUID" do
      property \(pgUuid :: Uuid.Uuid) ->
        let uuid = Uuid.toUUID pgUuid
            pgUuid' = Uuid.fromUUID uuid
         in pgUuid' === pgUuid
