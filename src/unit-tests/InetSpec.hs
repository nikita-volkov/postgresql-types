module InetSpec (spec) where

import Data.Maybe
import Data.Word
import qualified PostgresqlTypes.Types.Inet as Inet
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Inet" do
    describe "IPv4 Constructors" do
      describe "normalizeFromV4" do
        it "clamps netmask values above maximum (32)" do
          let inet = Inet.normalizeFromV4 0x7F000001 50
              (_, netmask) = fromJust (Inet.refineToV4 inet)
          netmask `shouldBe` 32

        it "accepts netmask at maximum (32)" do
          let inet = Inet.normalizeFromV4 0x7F000001 32
              (_, netmask) = fromJust (Inet.refineToV4 inet)
          netmask `shouldBe` 32

        it "accepts netmask at minimum (0)" do
          let inet = Inet.normalizeFromV4 0x7F000001 0
              (_, netmask) = fromJust (Inet.refineToV4 inet)
          netmask `shouldBe` 0

        it "preserves address and netmask within valid range" do
          let addr = 0xC0A80001 -- 192.168.0.1
              inet = Inet.normalizeFromV4 addr 24
              (addr', netmask) = fromJust (Inet.refineToV4 inet)
          addr' `shouldBe` addr
          netmask `shouldBe` 24

      describe "refineFromV4" do
        it "rejects netmask values above maximum (32)" do
          Inet.refineFromV4 0x7F000001 33 `shouldBe` Nothing

        it "accepts netmask at maximum (32)" do
          let result = Inet.refineFromV4 0x7F000001 32
          result `shouldSatisfy` isJust

        it "accepts netmask at minimum (0)" do
          let result = Inet.refineFromV4 0x7F000001 0
          result `shouldSatisfy` isJust

        it "preserves address and netmask for valid values" do
          let addr = 0xC0A80001 -- 192.168.0.1
              result = Inet.refineFromV4 addr 24
          case result of
            Just inet -> do
              let (addr', netmask) = fromJust (Inet.refineToV4 inet)
              addr' `shouldBe` addr
              netmask `shouldBe` 24
            Nothing -> expectationFailure "Expected Just but got Nothing"

    describe "IPv6 Constructors" do
      describe "normalizeFromV6" do
        it "clamps netmask values above maximum (128)" do
          let inet = Inet.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 200
              (_, _, _, _, netmask) = fromJust (Inet.refineToV6 inet)
          netmask `shouldBe` 128

        it "accepts netmask at maximum (128)" do
          let inet = Inet.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 128
              (_, _, _, _, netmask) = fromJust (Inet.refineToV6 inet)
          netmask `shouldBe` 128

        it "accepts netmask at minimum (0)" do
          let inet = Inet.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 0
              (_, _, _, _, netmask) = fromJust (Inet.refineToV6 inet)
          netmask `shouldBe` 0

        it "preserves address and netmask within valid range" do
          let w1 = 0x20010DB8
              w2 = 0x00000000
              w3 = 0x00000000
              w4 = 0x00000001
              inet = Inet.normalizeFromV6 w1 w2 w3 w4 64
              (w1', w2', w3', w4', netmask) = fromJust (Inet.refineToV6 inet)
          w1' `shouldBe` w1
          w2' `shouldBe` w2
          w3' `shouldBe` w3
          w4' `shouldBe` w4
          netmask `shouldBe` 64

      describe "refineFromV6" do
        it "rejects netmask values above maximum (128)" do
          Inet.refineFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 129 `shouldBe` Nothing

        it "accepts netmask at maximum (128)" do
          let result = Inet.refineFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 128
          result `shouldSatisfy` isJust

        it "accepts netmask at minimum (0)" do
          let result = Inet.refineFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 0
          result `shouldSatisfy` isJust

        it "preserves address and netmask for valid values" do
          let w1 = 0x20010DB8
              w2 = 0x00000000
              w3 = 0x00000000
              w4 = 0x00000001
              result = Inet.refineFromV6 w1 w2 w3 w4 64
          case result of
            Just inet -> do
              let (w1', w2', w3', w4', netmask) = fromJust (Inet.refineToV6 inet)
              w1' `shouldBe` w1
              w2' `shouldBe` w2
              w3' `shouldBe` w3
              w4' `shouldBe` w4
              netmask `shouldBe` 64
            Nothing -> expectationFailure "Expected Just but got Nothing"

    describe "Accessors" do
      describe "refineToV4" do
        it "extracts IPv4 address and netmask" do
          let addr = 0xC0A80001 -- 192.168.0.1
              inet = Inet.normalizeFromV4 addr 24
              result = Inet.refineToV4 inet
          case result of
            Just (addr', netmask) -> do
              addr' `shouldBe` addr
              netmask `shouldBe` 24
            Nothing -> expectationFailure "Expected Just for IPv4"

        it "returns Nothing for IPv6 address" do
          let inet = Inet.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 64
          Inet.refineToV4 inet `shouldBe` Nothing

      describe "refineToV6" do
        it "extracts IPv6 address and netmask" do
          let w1 = 0x20010DB8
              w2 = 0x00000000
              w3 = 0x00000000
              w4 = 0x00000001
              inet = Inet.normalizeFromV6 w1 w2 w3 w4 64
              result = Inet.refineToV6 inet
          case result of
            Just (w1', w2', w3', w4', netmask) -> do
              w1' `shouldBe` w1
              w2' `shouldBe` w2
              w3' `shouldBe` w3
              w4' `shouldBe` w4
              netmask `shouldBe` 64
            Nothing -> expectationFailure "Expected Just for IPv6"

        it "returns Nothing for IPv4 address" do
          let inet = Inet.normalizeFromV4 0xC0A80001 24
          Inet.refineToV6 inet `shouldBe` Nothing

      describe "fold" do
        it "handles IPv4 case" do
          let inet = Inet.normalizeFromV4 0x7F000001 8
              result =
                Inet.fold
                  (\addr netmask -> Left (addr, netmask))
                  (\_ _ _ _ _ -> Right ())
                  inet
          result `shouldBe` Left (0x7F000001, 8)

        it "handles IPv6 case" do
          let inet = Inet.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 64
              result =
                Inet.fold
                  (\_ _ -> Left ())
                  (\w1 w2 w3 w4 netmask -> Right (w1, w2, w3, w4, netmask))
                  inet
          result `shouldBe` Right (0x20010DB8, 0x00000000, 0x00000000, 0x00000001, 64)

    describe "Property Tests" do
      it "normalizeFromV4 is idempotent for valid netmasks" do
        QuickCheck.property \addr netmask ->
          let normalized1 = Inet.normalizeFromV4 addr netmask
              (addr', netmask') = fromJust (Inet.refineToV4 normalized1)
              normalized2 = Inet.normalizeFromV4 addr' netmask'
           in normalized1 === normalized2

      it "normalizeFromV6 is idempotent for valid netmasks" do
        QuickCheck.property \w1 w2 w3 w4 netmask ->
          let normalized1 = Inet.normalizeFromV6 w1 w2 w3 w4 netmask
              (w1', w2', w3', w4', netmask') = fromJust (Inet.refineToV6 normalized1)
              normalized2 = Inet.normalizeFromV6 w1' w2' w3' w4' netmask'
           in normalized1 === normalized2

      it "refineFromV4 accepts only valid netmasks" do
        QuickCheck.property \addr netmask ->
          let result = Inet.refineFromV4 addr netmask
           in isJust result === (netmask <= 32)

      it "refineFromV6 accepts only valid netmasks" do
        QuickCheck.property \w1 w2 w3 w4 netmask ->
          let result = Inet.refineFromV6 w1 w2 w3 w4 netmask
           in isJust result === (netmask <= 128)

      it "refineToV4 roundtrips with normalizeFromV4" do
        QuickCheck.property \addr (netmask :: Word8) ->
          netmask <= 32 ==>
            let inet = Inet.normalizeFromV4 addr netmask
                result = Inet.refineToV4 inet
             in result === Just (addr, netmask)

      it "refineToV6 roundtrips with normalizeFromV6" do
        QuickCheck.property \w1 w2 w3 w4 (netmask :: Word8) ->
          netmask <= 128 ==>
            let inet = Inet.normalizeFromV6 w1 w2 w3 w4 netmask
                result = Inet.refineToV6 inet
             in result === Just (w1, w2, w3, w4, netmask)

      it "fold correctly identifies IPv4" do
        QuickCheck.property \addr (netmask :: Word8) ->
          netmask <= 32 ==>
            let inet = Inet.normalizeFromV4 addr netmask
                isV4 = Inet.fold (\_ _ -> True) (\_ _ _ _ _ -> False) inet
             in isV4 === True

      it "fold correctly identifies IPv6" do
        QuickCheck.property \w1 w2 w3 w4 (netmask :: Word8) ->
          netmask <= 128 ==>
            let inet = Inet.normalizeFromV6 w1 w2 w3 w4 netmask
                isV6 = Inet.fold (\_ _ -> False) (\_ _ _ _ _ -> True) inet
             in isV6 === True
