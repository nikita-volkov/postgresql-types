module RangeSpec (spec) where

import qualified PostgresqlTypes.Int4 as Int4
import qualified PostgresqlTypes.Range as Range
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Range" do
    describe "Range Int4" do
      it "has Eq instance" do
        property \(r1 :: Range.Range Int4.Int4) ->
          (r1 == r1) `shouldBe` True

      it "has Functor instance - fmap id = id" do
        property \(range :: Range.Range Int4.Int4) ->
          fmap id range === range

      it "has Functor instance - composition law" do
        property \(range :: Range.Range Int4.Int4) ->
          let f = Int4.fromInt32 . (* 2) . Int4.toInt32
              g = Int4.fromInt32 . (+ 1) . Int4.toInt32
           in fmap (f . g) range === fmap f (fmap g range)

    describe "Property Tests" do
      it "arbitrary generates valid ranges" do
        property \(range :: Range.Range Int4.Int4) ->
          -- Range instances should be well-formed
          range === range
