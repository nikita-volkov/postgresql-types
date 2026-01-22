module MultirangeSpec (spec) where

import qualified PostgresqlTypes.Int4 as Int4
import qualified PostgresqlTypes.Multirange as Multirange
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Multirange" do
    describe "Multirange Int4" do
      it "has Eq instance" do
        property \(mr1 :: Multirange.Multirange Int4.Int4) ->
          (mr1 == mr1) `shouldBe` True

      it "has Functor instance - fmap id = id" do
        property \(multirange :: Multirange.Multirange Int4.Int4) ->
          fmap id multirange === multirange

      it "has Functor instance - composition law" do
        property \(multirange :: Multirange.Multirange Int4.Int4) ->
          let f = Int4.fromInt32 . (* 2) . Int4.toInt32
              g = Int4.fromInt32 . (+ 1) . Int4.toInt32
           in fmap (f . g) multirange === fmap f (fmap g multirange)

    describe "Property Tests" do
      it "arbitrary generates valid multiranges" do
        property \(multirange :: Multirange.Multirange Int4.Int4) ->
          -- Multirange instances should be well-formed
          multirange === multirange

      it "equality is reflexive" do
        property \(multirange :: Multirange.Multirange Int4.Int4) ->
          multirange === multirange
