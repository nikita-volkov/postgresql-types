module MacaddrSpec (spec) where

import Data.Word
import qualified PostgresqlTypes.Macaddr as Macaddr
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Macaddr" do
    describe "Constructors" do
      describe "fromBytes" do
        it "creates Macaddr from 6 bytes" do
          let macaddr = Macaddr.fromBytes (0x01, 0x23, 0x45, 0x67, 0x89, 0xAB)
              (b1, b2, b3, b4, b5, b6) = Macaddr.toBytes macaddr
          (b1, b2, b3, b4, b5, b6) `shouldBe` (0x01, 0x23, 0x45, 0x67, 0x89, 0xAB)

        it "creates Macaddr from all zeros" do
          let macaddr = Macaddr.fromBytes (0, 0, 0, 0, 0, 0)
              (b1, b2, b3, b4, b5, b6) = Macaddr.toBytes macaddr
          (b1, b2, b3, b4, b5, b6) `shouldBe` (0, 0, 0, 0, 0, 0)

        it "creates Macaddr from all 0xFF" do
          let macaddr = Macaddr.fromBytes (0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF)
              (b1, b2, b3, b4, b5, b6) = Macaddr.toBytes macaddr
          (b1, b2, b3, b4, b5, b6) `shouldBe` (0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF)

    describe "Accessors" do
      describe "toBytes" do
        it "extracts 6 bytes as tuple" do
          let macaddr = Macaddr.fromBytes (0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC)
              (b1, b2, b3, b4, b5, b6) = Macaddr.toBytes macaddr
          (b1, b2, b3, b4, b5, b6) `shouldBe` (0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC)

    describe "Property Tests" do
      it "roundtrips through toBytes and fromBytes" do
        property \(b1 :: Word8, b2 :: Word8, b3 :: Word8, b4 :: Word8, b5 :: Word8, b6 :: Word8) ->
          let macaddr = Macaddr.fromBytes (b1, b2, b3, b4, b5, b6)
              (b1', b2', b3', b4', b5', b6') = Macaddr.toBytes macaddr
           in (b1', b2', b3', b4', b5', b6') === (b1, b2, b3, b4, b5, b6)

      it "roundtrips through fromBytes and toBytes" do
        property \(macaddr :: Macaddr.Macaddr) ->
          let bytes = Macaddr.toBytes macaddr
              macaddr' = Macaddr.fromBytes bytes
           in macaddr' === macaddr
