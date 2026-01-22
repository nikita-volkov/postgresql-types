module Macaddr8Spec (spec) where

import Data.Word
import qualified PostgresqlTypes.Macaddr8 as Macaddr8
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Macaddr8" do
    describe "Constructors" do
      describe "fromBytes" do
        it "creates Macaddr8 from 8 bytes" do
          let macaddr8 = Macaddr8.fromBytes (0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF)
              (b1, b2, b3, b4, b5, b6, b7, b8) = Macaddr8.toBytes macaddr8
          (b1, b2, b3, b4, b5, b6, b7, b8) `shouldBe` (0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF)

        it "creates Macaddr8 from all zeros" do
          let macaddr8 = Macaddr8.fromBytes (0, 0, 0, 0, 0, 0, 0, 0)
              (b1, b2, b3, b4, b5, b6, b7, b8) = Macaddr8.toBytes macaddr8
          (b1, b2, b3, b4, b5, b6, b7, b8) `shouldBe` (0, 0, 0, 0, 0, 0, 0, 0)

        it "creates Macaddr8 from all 0xFF" do
          let macaddr8 = Macaddr8.fromBytes (0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF)
              (b1, b2, b3, b4, b5, b6, b7, b8) = Macaddr8.toBytes macaddr8
          (b1, b2, b3, b4, b5, b6, b7, b8) `shouldBe` (0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF)

    describe "Accessors" do
      describe "toBytes" do
        it "extracts 8 bytes as tuple" do
          let macaddr8 = Macaddr8.fromBytes (0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0)
              (b1, b2, b3, b4, b5, b6, b7, b8) = Macaddr8.toBytes macaddr8
          (b1, b2, b3, b4, b5, b6, b7, b8) `shouldBe` (0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0)

    describe "Property Tests" do
      it "roundtrips through toBytes and fromBytes" do
        property \(b1 :: Word8, b2 :: Word8, b3 :: Word8, b4 :: Word8, b5 :: Word8, b6 :: Word8, b7 :: Word8, b8 :: Word8) ->
          let macaddr8 = Macaddr8.fromBytes (b1, b2, b3, b4, b5, b6, b7, b8)
              (b1', b2', b3', b4', b5', b6', b7', b8') = Macaddr8.toBytes macaddr8
           in (b1', b2', b3', b4', b5', b6', b7', b8') === (b1, b2, b3, b4, b5, b6, b7, b8)

      it "roundtrips through fromBytes and toBytes" do
        property \(macaddr8 :: Macaddr8.Macaddr8) ->
          let bytes = Macaddr8.toBytes macaddr8
              macaddr8' = Macaddr8.fromBytes bytes
           in macaddr8' === macaddr8
