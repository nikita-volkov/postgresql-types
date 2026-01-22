module PostgresqlTypes.BitSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified PostgresqlTypes.Bit as Bit
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @(Bit.Bit 0))
    Scripts.testShowRead (Proxy @(Bit.Bit 1))
    Scripts.testShowRead (Proxy @(Bit.Bit 8))

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @(Bit.Bit 0))
    Scripts.testIsScalar (Proxy @(Bit.Bit 1))
    Scripts.testIsScalar (Proxy @(Bit.Bit 64))

  describe "Bit 8" do
    describe "Constructors" do
      describe "normalizeFromBoolList" do
        it "creates Bit from list of correct length" do
          let bit = Bit.normalizeFromBoolList @8 [True, False, True, False, True, False, True, False]
          Bit.toBoolList bit `shouldBe` [True, False, True, False, True, False, True, False]

        it "truncates list longer than required" do
          let bit = Bit.normalizeFromBoolList @8 [True, False, True, False, True, False, True, False, True, True]
          length (Bit.toBoolList bit) `shouldBe` 8

        it "pads list shorter than required" do
          let bit = Bit.normalizeFromBoolList @8 [True, False, True]
          length (Bit.toBoolList bit) `shouldBe` 8

      describe "refineFromBoolList" do
        it "rejects list with incorrect length" do
          Bit.refineFromBoolList @8 [True, False, True] `shouldBe` Nothing
          Bit.refineFromBoolList @8 (replicate 10 True) `shouldBe` Nothing

        it "accepts list with correct length" do
          let result = Bit.refineFromBoolList @8 (replicate 8 True)
          result `shouldSatisfy` isJust

      describe "normalizeFromBoolVector" do
        it "creates Bit from vector" do
          let vec = VU.fromList [True, False, True, False, True, False, True, False]
              bit = Bit.normalizeFromBoolVector @8 vec
          Bit.toBoolVector bit `shouldBe` vec

    describe "Accessors" do
      describe "toBoolList" do
        it "extracts Bool list" do
          let bit = Bit.normalizeFromBoolList @8 [True, True, False, False, True, True, False, False]
          Bit.toBoolList bit `shouldBe` [True, True, False, False, True, True, False, False]

      describe "toBoolVector" do
        it "extracts Bool vector" do
          let bools = [True, False, True, False, True, False, True, False]
              bit = Bit.normalizeFromBoolList @8 bools
          VU.toList (Bit.toBoolVector bit) `shouldBe` bools

    describe "Property Tests" do
      it "roundtrips through toBoolList and normalizeFromBoolList" do
        property \(bit :: Bit.Bit 8) ->
          let bools = Bit.toBoolList bit
              bit' = Bit.normalizeFromBoolList @8 bools
           in bit' === bit

      it "roundtrips through toBoolVector and normalizeFromBoolVector" do
        property \(bit :: Bit.Bit 8) ->
          let vec = Bit.toBoolVector bit :: VU.Vector Bool
              bit' = Bit.normalizeFromBoolVector @8 vec
           in bit' === bit
