module PostgresqlTypes.VarbitSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified PostgresqlTypes.Varbit as Varbit
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @(Varbit.Varbit 16))

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @(Varbit.Varbit 16))

  describe "Varbit 16" do
    describe "Constructors" do
      describe "normalizeFromBoolList" do
        it "creates Varbit from list within max length" do
          let varbit = Varbit.normalizeFromBoolList @16 [True, False, True, False, True]
          Varbit.toBoolList varbit `shouldBe` [True, False, True, False, True]

        it "truncates list exceeding max length" do
          let varbit = Varbit.normalizeFromBoolList @16 (replicate 20 True)
          length (Varbit.toBoolList varbit) `shouldBe` 16

        it "accepts empty list" do
          let varbit = Varbit.normalizeFromBoolList @16 []
          Varbit.toBoolList varbit `shouldBe` []

      describe "refineFromBoolList" do
        it "rejects list exceeding max length" do
          Varbit.refineFromBoolList @16 (replicate 20 True) `shouldBe` Nothing

        it "accepts list within max length" do
          let result = Varbit.refineFromBoolList @16 [True, False, True]
          result `shouldSatisfy` isJust

        it "accepts empty list" do
          let result = Varbit.refineFromBoolList @16 []
          result `shouldSatisfy` isJust

      describe "normalizeFromBoolVector" do
        it "creates Varbit from vector" do
          let vec = VU.fromList [True, False, True, False]
              varbit = Varbit.normalizeFromBoolVector @16 vec
          Varbit.toBoolVector varbit `shouldBe` vec

    describe "Accessors" do
      describe "toBoolList" do
        it "extracts Bool list" do
          let varbit = Varbit.normalizeFromBoolList @16 [True, True, False]
          Varbit.toBoolList varbit `shouldBe` [True, True, False]

      describe "toBoolList" do
        it "extracts Bool list" do
          let bools = [True, False, True, False]
              varbit = Varbit.normalizeFromBoolList @16 bools
          Varbit.toBoolList varbit `shouldBe` bools

    describe "Property Tests" do
      it "roundtrips through toBoolList and normalizeFromBoolList" do
        property \(varbit :: Varbit.Varbit 16) ->
          let bools = Varbit.toBoolList varbit
              varbit' = Varbit.normalizeFromBoolList @16 bools
           in varbit' === varbit

      it "roundtrips through toBoolList and normalizeFromBoolVector" do
        property \(varbit :: Varbit.Varbit 16) ->
          let vec = Varbit.toBoolVector @_ @VU.Vector varbit
              varbit' = Varbit.normalizeFromBoolVector @16 vec
           in varbit' === varbit
