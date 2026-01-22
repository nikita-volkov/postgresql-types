module PostgresqlTypes.ByteaSpec (spec) where

import qualified Data.ByteString as ByteString
import qualified PostgresqlTypes.Bytea as Bytea
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Bytea" do
    describe "Constructors" do
      describe "fromByteString" do
        it "creates Bytea from ByteString" do
          let bs = ByteString.pack [1, 2, 3, 4, 5]
              pgBytea = Bytea.fromByteString bs
          Bytea.toByteString pgBytea `shouldBe` bs

        it "creates Bytea from empty ByteString" do
          let bs = ByteString.empty
              pgBytea = Bytea.fromByteString bs
          Bytea.toByteString pgBytea `shouldBe` bs

        it "preserves binary data" do
          let bs = ByteString.pack [0, 255, 128, 1, 127]
              pgBytea = Bytea.fromByteString bs
          Bytea.toByteString pgBytea `shouldBe` bs

    describe "Accessors" do
      describe "toByteString" do
        it "extracts ByteString value" do
          let bs = ByteString.pack [10, 20, 30]
              pgBytea = Bytea.fromByteString bs
          Bytea.toByteString pgBytea `shouldBe` bs

    describe "Property Tests" do
      it "roundtrips through toByteString and fromByteString" do
        property \(bs :: ByteString.ByteString) ->
          let pgBytea = Bytea.fromByteString bs
           in Bytea.toByteString pgBytea === bs

      it "roundtrips through fromByteString and toByteString" do
        property \(pgBytea :: Bytea.Bytea) ->
          let bs = Bytea.toByteString pgBytea
              pgBytea' = Bytea.fromByteString bs
           in pgBytea' === pgBytea
