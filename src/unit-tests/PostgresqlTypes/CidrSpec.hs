module PostgresqlTypes.CidrSpec (spec) where

import Data.Bits
import Data.Maybe
import Data.Word
import qualified PostgresqlTypes.Cidr as Cidr
import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Prelude

spec :: Spec
spec = do
  describe "Cidr" do
    describe "IPv4 Constructors" do
      describe "normalizeFromV4" do
        it "clamps netmask values above maximum (32)" do
          let cidr = Cidr.normalizeFromV4 0xC0A80000 50
              (_, netmask) = fromJust (Cidr.refineToV4 cidr)
          netmask `shouldBe` 32

        it "accepts netmask at maximum (32)" do
          let cidr = Cidr.normalizeFromV4 0xC0A80000 32
              (_, netmask) = fromJust (Cidr.refineToV4 cidr)
          netmask `shouldBe` 32

        it "accepts netmask at minimum (0)" do
          let cidr = Cidr.normalizeFromV4 0xC0A80000 0
              (_, netmask) = fromJust (Cidr.refineToV4 cidr)
          netmask `shouldBe` 0

        it "zeros out host bits for /24 network" do
          let cidr = Cidr.normalizeFromV4 0xC0A80001 24 -- 192.168.0.1/24
              (addr, netmask) = fromJust (Cidr.refineToV4 cidr)
          addr `shouldBe` 0xC0A80000 -- Should be 192.168.0.0
          netmask `shouldBe` 24

        it "preserves network address when host bits are already zero" do
          let addr = 0xC0A80000 -- 192.168.0.0
              cidr = Cidr.normalizeFromV4 addr 24
              (addr', netmask) = fromJust (Cidr.refineToV4 cidr)
          addr' `shouldBe` addr
          netmask `shouldBe` 24

      describe "refineFromV4" do
        it "rejects netmask values above maximum (32)" do
          Cidr.refineFromV4 0xC0A80000 33 `shouldBe` Nothing

        it "accepts netmask at maximum (32)" do
          let result = Cidr.refineFromV4 0xC0A80000 32
          result `shouldSatisfy` isJust

        it "accepts netmask at minimum (0)" do
          let result = Cidr.refineFromV4 0x00000000 0
          result `shouldSatisfy` isJust

        it "rejects address with non-zero host bits" do
          let result = Cidr.refineFromV4 0xC0A80001 24 -- 192.168.0.1/24
          result `shouldBe` Nothing

        it "accepts address with zero host bits" do
          let addr = 0xC0A80000 -- 192.168.0.0/24
              result = Cidr.refineFromV4 addr 24
          case result of
            Just cidr -> do
              let (addr', netmask) = fromJust (Cidr.refineToV4 cidr)
              addr' `shouldBe` addr
              netmask `shouldBe` 24
            Nothing -> expectationFailure "Expected Just but got Nothing"

    describe "IPv6 Constructors" do
      describe "normalizeFromV6" do
        it "clamps netmask values above maximum (128)" do
          let cidr = Cidr.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000000 200
              (_, _, _, _, netmask) = fromJust (Cidr.refineToV6 cidr)
          netmask `shouldBe` 128

        it "accepts netmask at maximum (128)" do
          let cidr = Cidr.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000000 128
              (_, _, _, _, netmask) = fromJust (Cidr.refineToV6 cidr)
          netmask `shouldBe` 128

        it "accepts netmask at minimum (0)" do
          let cidr = Cidr.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000000 0
              (_, _, _, _, netmask) = fromJust (Cidr.refineToV6 cidr)
          netmask `shouldBe` 0

        it "zeros out host bits for /64 network" do
          let cidr = Cidr.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 64 -- 2001:db8::/64 with host bits
              (w1, w2, w3, w4, netmask) = fromJust (Cidr.refineToV6 cidr)
          w1 `shouldBe` 0x20010DB8
          w2 `shouldBe` 0x00000000
          w3 `shouldBe` 0x00000000
          w4 `shouldBe` 0x00000000 -- Host bits should be zero
          netmask `shouldBe` 64

        it "preserves network address when host bits are already zero" do
          let w1 = 0x20010DB8
              w2 = 0x00000000
              w3 = 0x00000000
              w4 = 0x00000000
              cidr = Cidr.normalizeFromV6 w1 w2 w3 w4 64
              (w1', w2', w3', w4', netmask) = fromJust (Cidr.refineToV6 cidr)
          w1' `shouldBe` w1
          w2' `shouldBe` w2
          w3' `shouldBe` w3
          w4' `shouldBe` w4
          netmask `shouldBe` 64

      describe "refineFromV6" do
        it "rejects netmask values above maximum (128)" do
          Cidr.refineFromV6 0x20010DB8 0x00000000 0x00000000 0x00000000 129 `shouldBe` Nothing

        it "accepts netmask at maximum (128)" do
          let result = Cidr.refineFromV6 0x20010DB8 0x00000000 0x00000000 0x00000000 128
          result `shouldSatisfy` isJust

        it "accepts netmask at minimum (0)" do
          let result = Cidr.refineFromV6 0x00000000 0x00000000 0x00000000 0x00000000 0
          result `shouldSatisfy` isJust

        it "rejects address with non-zero host bits" do
          let result = Cidr.refineFromV6 0x20010DB8 0x00000000 0x00000000 0x00000001 64 -- Has host bits
          result `shouldBe` Nothing

        it "accepts address with zero host bits" do
          let w1 = 0x20010DB8
              w2 = 0x00000000
              w3 = 0x00000000
              w4 = 0x00000000
              result = Cidr.refineFromV6 w1 w2 w3 w4 64
          case result of
            Just cidr -> do
              let (w1', w2', w3', w4', netmask) = fromJust (Cidr.refineToV6 cidr)
              w1' `shouldBe` w1
              w2' `shouldBe` w2
              w3' `shouldBe` w3
              w4' `shouldBe` w4
              netmask `shouldBe` 64
            Nothing -> expectationFailure "Expected Just but got Nothing"

    describe "Accessors" do
      describe "refineToV4" do
        it "extracts IPv4 network address and netmask" do
          let addr = 0xC0A80000 -- 192.168.0.0
              cidr = Cidr.normalizeFromV4 addr 24
              result = Cidr.refineToV4 cidr
          case result of
            Just (addr', netmask) -> do
              addr' `shouldBe` addr
              netmask `shouldBe` 24
            Nothing -> expectationFailure "Expected Just for IPv4"

        it "returns Nothing for IPv6 address" do
          let cidr = Cidr.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000000 64
          Cidr.refineToV4 cidr `shouldBe` Nothing

      describe "refineToV6" do
        it "extracts IPv6 network address and netmask" do
          let w1 = 0x20010DB8
              w2 = 0x00000000
              w3 = 0x00000000
              w4 = 0x00000000
              cidr = Cidr.normalizeFromV6 w1 w2 w3 w4 64
              result = Cidr.refineToV6 cidr
          case result of
            Just (w1', w2', w3', w4', netmask) -> do
              w1' `shouldBe` w1
              w2' `shouldBe` w2
              w3' `shouldBe` w3
              w4' `shouldBe` w4
              netmask `shouldBe` 64
            Nothing -> expectationFailure "Expected Just for IPv6"

        it "returns Nothing for IPv4 address" do
          let cidr = Cidr.normalizeFromV4 0xC0A80000 24
          Cidr.refineToV6 cidr `shouldBe` Nothing

      describe "fold" do
        it "handles IPv4 case" do
          let cidr = Cidr.normalizeFromV4 0xC0A80000 24
              result =
                Cidr.fold
                  (\addr netmask -> Left (addr, netmask))
                  (\_ _ _ _ _ -> Right ())
                  cidr
          result `shouldBe` Left (0xC0A80000, 24)

        it "handles IPv6 case" do
          let cidr = Cidr.normalizeFromV6 0x20010DB8 0x00000000 0x00000000 0x00000000 64
              result =
                Cidr.fold
                  (\_ _ -> Left ())
                  (\w1 w2 w3 w4 netmask -> Right (w1, w2, w3, w4, netmask))
                  cidr
          result `shouldBe` Right (0x20010DB8, 0x00000000, 0x00000000, 0x00000000, 64)

    describe "Property Tests" do
      it "normalizeFromV4 always zeros host bits" do
        property \addr netmask ->
          let cidr = Cidr.normalizeFromV4 addr netmask
              (addr', _) = fromJust (Cidr.refineToV4 cidr)
              clampedNetmask = min 32 netmask
              hostBits = 32 - fromIntegral clampedNetmask
              networkMask = if hostBits >= 32 then 0 else complement ((1 `shiftL` hostBits) - 1)
              expectedAddr = addr .&. networkMask
           in addr' === expectedAddr

      it "normalizeFromV6 always zeros host bits" do
        property \w1 w2 w3 w4 netmask ->
          let cidr = Cidr.normalizeFromV6 w1 w2 w3 w4 netmask
              (w1', w2', w3', w4', _) = fromJust (Cidr.refineToV6 cidr)
              -- Verify host bits are zero by attempting to refine
              refined = Cidr.refineFromV6 w1' w2' w3' w4' (min 128 netmask)
           in isJust refined === True

      it "normalizeFromV4 is idempotent" do
        property \addr netmask ->
          let normalized1 = Cidr.normalizeFromV4 addr netmask
              (addr', netmask') = fromJust (Cidr.refineToV4 normalized1)
              normalized2 = Cidr.normalizeFromV4 addr' netmask'
           in normalized1 === normalized2

      it "normalizeFromV6 is idempotent" do
        property \w1 w2 w3 w4 netmask ->
          let normalized1 = Cidr.normalizeFromV6 w1 w2 w3 w4 netmask
              (w1', w2', w3', w4', netmask') = fromJust (Cidr.refineToV6 normalized1)
              normalized2 = Cidr.normalizeFromV6 w1' w2' w3' w4' netmask'
           in normalized1 === normalized2

      it "refineFromV4 accepts only valid network addresses" do
        property \addr netmask ->
          let result = Cidr.refineFromV4 addr netmask
              isValid =
                netmask <= 32
                  && ( let hostBits = 32 - fromIntegral netmask
                           networkMask = if hostBits >= 32 then 0 else complement ((1 `shiftL` hostBits) - 1)
                           networkAddr = addr .&. networkMask
                        in networkAddr == addr
                     )
           in isJust result === isValid

      it "refineFromV6 accepts only valid network addresses" do
        property \w1 w2 w3 w4 netmask ->
          netmask <= 128 ==>
            let result = Cidr.refineFromV6 w1 w2 w3 w4 netmask
                normalized = Cidr.normalizeFromV6 w1 w2 w3 w4 netmask
                (nw1, nw2, nw3, nw4, _) = fromJust (Cidr.refineToV6 normalized)
             in isJust result === ((w1, w2, w3, w4) == (nw1, nw2, nw3, nw4))

      it "refineToV4 roundtrips with normalizeFromV4" do
        property \addr (netmask :: Word8) ->
          netmask <= 32 ==>
            let cidr = Cidr.normalizeFromV4 addr netmask
                result = Cidr.refineToV4 cidr
                (addr', netmask') = fromJust result
                -- The address should be normalized (host bits zero)
                clampedNetmask = min 32 netmask
                hostBits = 32 - fromIntegral clampedNetmask
                networkMask = if hostBits >= 32 then 0 else complement ((1 `shiftL` hostBits) - 1)
                expectedAddr = addr .&. networkMask
             in (addr', netmask') === (expectedAddr, clampedNetmask)

      it "refineToV6 roundtrips with normalizeFromV6" do
        property \w1 w2 w3 w4 (netmask :: Word8) ->
          netmask <= 128 ==>
            let cidr = Cidr.normalizeFromV6 w1 w2 w3 w4 netmask
                result = Cidr.refineToV6 cidr
                (_, _, _, _, netmask') = fromJust result
             in netmask' === min 128 netmask

      it "fold correctly identifies IPv4" do
        property \addr (netmask :: Word8) ->
          netmask <= 32 ==>
            let cidr = Cidr.normalizeFromV4 addr netmask
                isV4 = Cidr.fold (\_ _ -> True) (\_ _ _ _ _ -> False) cidr
             in isV4 === True

      it "fold correctly identifies IPv6" do
        property \w1 w2 w3 w4 (netmask :: Word8) ->
          netmask <= 128 ==>
            let cidr = Cidr.normalizeFromV6 w1 w2 w3 w4 netmask
                isV6 = Cidr.fold (\_ _ -> False) (\_ _ _ _ _ -> True) cidr
             in isV6 === True
