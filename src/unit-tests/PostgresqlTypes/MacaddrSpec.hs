module PostgresqlTypes.MacaddrSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Word
import qualified PostgresqlTypes.Macaddr as Macaddr
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Macaddr.Macaddr)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @Macaddr.Macaddr)

  describe "Constructors" do
    describe "fromBytes" do
      it "creates Macaddr from 6 bytes" do
        let macaddr = Macaddr.fromBytes 0x01 0x23 0x45 0x67 0x89 0xAB
        Macaddr.toByte1 macaddr `shouldBe` 0x01
        Macaddr.toByte2 macaddr `shouldBe` 0x23
        Macaddr.toByte3 macaddr `shouldBe` 0x45
        Macaddr.toByte4 macaddr `shouldBe` 0x67
        Macaddr.toByte5 macaddr `shouldBe` 0x89
        Macaddr.toByte6 macaddr `shouldBe` 0xAB

      it "creates Macaddr from all zeros" do
        let macaddr = Macaddr.fromBytes 0 0 0 0 0 0
        (Macaddr.toByte1 macaddr, Macaddr.toByte2 macaddr, Macaddr.toByte3 macaddr)
          `shouldBe` (0, 0, 0)
        (Macaddr.toByte4 macaddr, Macaddr.toByte5 macaddr, Macaddr.toByte6 macaddr)
          `shouldBe` (0, 0, 0)

      it "creates Macaddr from all 0xFF" do
        let macaddr = Macaddr.fromBytes 0xFF 0xFF 0xFF 0xFF 0xFF 0xFF
        (Macaddr.toByte1 macaddr, Macaddr.toByte2 macaddr, Macaddr.toByte3 macaddr)
          `shouldBe` (0xFF, 0xFF, 0xFF)
        (Macaddr.toByte4 macaddr, Macaddr.toByte5 macaddr, Macaddr.toByte6 macaddr)
          `shouldBe` (0xFF, 0xFF, 0xFF)

  describe "Accessors" do
    it "extracts individual bytes" do
      let macaddr = Macaddr.fromBytes 0x12 0x34 0x56 0x78 0x9A 0xBC
      Macaddr.toByte1 macaddr `shouldBe` 0x12
      Macaddr.toByte2 macaddr `shouldBe` 0x34
      Macaddr.toByte3 macaddr `shouldBe` 0x56
      Macaddr.toByte4 macaddr `shouldBe` 0x78
      Macaddr.toByte5 macaddr `shouldBe` 0x9A
      Macaddr.toByte6 macaddr `shouldBe` 0xBC

  describe "Property Tests" do
    it "roundtrips through accessors and fromBytes" do
      property \(b1 :: Word8, b2 :: Word8, b3 :: Word8, b4 :: Word8, b5 :: Word8, b6 :: Word8) ->
        let macaddr = Macaddr.fromBytes b1 b2 b3 b4 b5 b6
         in (Macaddr.toByte1 macaddr, Macaddr.toByte2 macaddr, Macaddr.toByte3 macaddr, Macaddr.toByte4 macaddr, Macaddr.toByte5 macaddr, Macaddr.toByte6 macaddr) === (b1, b2, b3, b4, b5, b6)

    it "roundtrips through fromBytes and accessors" do
      property \(macaddr :: Macaddr.Macaddr) ->
        let macaddr' = Macaddr.fromBytes (Macaddr.toByte1 macaddr) (Macaddr.toByte2 macaddr) (Macaddr.toByte3 macaddr) (Macaddr.toByte4 macaddr) (Macaddr.toByte5 macaddr) (Macaddr.toByte6 macaddr)
         in macaddr' === macaddr
