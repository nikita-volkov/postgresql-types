module PostgresqlTypes.Macaddr8Spec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Word
import qualified PostgresqlTypes.Macaddr8 as Macaddr8
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Macaddr8.Macaddr8)

  describe "Constructors" do
    describe "fromBytes" do
      it "creates Macaddr8 from 8 bytes" do
        let macaddr8 = Macaddr8.fromBytes 0x01 0x23 0x45 0x67 0x89 0xAB 0xCD 0xEF
        Macaddr8.toByte1 macaddr8 `shouldBe` 0x01
        Macaddr8.toByte2 macaddr8 `shouldBe` 0x23
        Macaddr8.toByte3 macaddr8 `shouldBe` 0x45
        Macaddr8.toByte4 macaddr8 `shouldBe` 0x67
        Macaddr8.toByte5 macaddr8 `shouldBe` 0x89
        Macaddr8.toByte6 macaddr8 `shouldBe` 0xAB
        Macaddr8.toByte7 macaddr8 `shouldBe` 0xCD
        Macaddr8.toByte8 macaddr8 `shouldBe` 0xEF

      it "creates Macaddr8 from all zeros" do
        let macaddr8 = Macaddr8.fromBytes 0 0 0 0 0 0 0 0
        (Macaddr8.toByte1 macaddr8, Macaddr8.toByte2 macaddr8, Macaddr8.toByte3 macaddr8, Macaddr8.toByte4 macaddr8)
          `shouldBe` (0, 0, 0, 0)
        (Macaddr8.toByte5 macaddr8, Macaddr8.toByte6 macaddr8, Macaddr8.toByte7 macaddr8, Macaddr8.toByte8 macaddr8)
          `shouldBe` (0, 0, 0, 0)

      it "creates Macaddr8 from all 0xFF" do
        let macaddr8 = Macaddr8.fromBytes 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF
        (Macaddr8.toByte1 macaddr8, Macaddr8.toByte2 macaddr8, Macaddr8.toByte3 macaddr8, Macaddr8.toByte4 macaddr8)
          `shouldBe` (0xFF, 0xFF, 0xFF, 0xFF)
        (Macaddr8.toByte5 macaddr8, Macaddr8.toByte6 macaddr8, Macaddr8.toByte7 macaddr8, Macaddr8.toByte8 macaddr8)
          `shouldBe` (0xFF, 0xFF, 0xFF, 0xFF)

  describe "Accessors" do
    it "extracts individual bytes" do
      let macaddr8 = Macaddr8.fromBytes 0x12 0x34 0x56 0x78 0x9A 0xBC 0xDE 0xF0
      Macaddr8.toByte1 macaddr8 `shouldBe` 0x12
      Macaddr8.toByte2 macaddr8 `shouldBe` 0x34
      Macaddr8.toByte3 macaddr8 `shouldBe` 0x56
      Macaddr8.toByte4 macaddr8 `shouldBe` 0x78
      Macaddr8.toByte5 macaddr8 `shouldBe` 0x9A
      Macaddr8.toByte6 macaddr8 `shouldBe` 0xBC
      Macaddr8.toByte7 macaddr8 `shouldBe` 0xDE
      Macaddr8.toByte8 macaddr8 `shouldBe` 0xF0

  describe "Property Tests" do
    it "roundtrips through accessors and fromBytes" do
      property \(b1 :: Word8, b2 :: Word8, b3 :: Word8, b4 :: Word8, b5 :: Word8, b6 :: Word8, b7 :: Word8, b8 :: Word8) ->
        let macaddr8 = Macaddr8.fromBytes b1 b2 b3 b4 b5 b6 b7 b8
         in (Macaddr8.toByte1 macaddr8, Macaddr8.toByte2 macaddr8, Macaddr8.toByte3 macaddr8, Macaddr8.toByte4 macaddr8, Macaddr8.toByte5 macaddr8, Macaddr8.toByte6 macaddr8, Macaddr8.toByte7 macaddr8, Macaddr8.toByte8 macaddr8) === (b1, b2, b3, b4, b5, b6, b7, b8)

    it "roundtrips through fromBytes and accessors" do
      property \(macaddr8 :: Macaddr8.Macaddr8) ->
        let macaddr8' = Macaddr8.fromBytes (Macaddr8.toByte1 macaddr8) (Macaddr8.toByte2 macaddr8) (Macaddr8.toByte3 macaddr8) (Macaddr8.toByte4 macaddr8) (Macaddr8.toByte5 macaddr8) (Macaddr8.toByte6 macaddr8) (Macaddr8.toByte7 macaddr8) (Macaddr8.toByte8 macaddr8)
         in macaddr8' === macaddr8
