module PostgresqlTypes.VarbitSpec (spec) where

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified PostgresqlTypes.Varbit as Varbit
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Varbit" do
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

        describe "toBoolVector" do
          it "extracts Bool vector" do
            let bools = [True, False, True, False]
                varbit = Varbit.normalizeFromBoolList @16 bools
            VU.toList (Varbit.toBoolVector varbit) `shouldBe` bools

      describe "Property Tests" do
        it "roundtrips through toBoolList and normalizeFromBoolList" do
          property \(varbit :: Varbit.Varbit 16) ->
            let bools = Varbit.toBoolList varbit
                varbit' = Varbit.normalizeFromBoolList @16 bools
             in varbit' === varbit

        it "roundtrips through toBoolVector and normalizeFromBoolVector" do
          property \(varbit :: Varbit.Varbit 16) ->
            let vec = Varbit.toBoolVector varbit
                varbit' = Varbit.normalizeFromBoolVector @16 vec
             in varbit' === varbit
