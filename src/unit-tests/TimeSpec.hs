module TimeSpec (spec) where

import Data.Int
import Data.Maybe
import qualified Data.Time as Time
import qualified PostgresqlTypes.Time as PgTime
import Test.Hspec
import Test.QuickCheck
import Prelude

spec :: Spec
spec = do
  describe "Time" do
    describe "Constructors" do
      describe "fromMicroseconds" do
        it "creates Time from microseconds" do
          let pgTime = PgTime.fromMicroseconds 43_200_000_000 -- noon
          PgTime.toMicroseconds pgTime `shouldBe` 43_200_000_000

        it "creates Time from zero (midnight)" do
          let pgTime = PgTime.fromMicroseconds 0
          PgTime.toMicroseconds pgTime `shouldBe` 0

      describe "normalizeFromTimeOfDay" do
        it "converts TimeOfDay to Time" do
          let tod = Time.TimeOfDay 12 30 45
              pgTime = PgTime.normalizeFromTimeOfDay tod
          PgTime.toTimeOfDay pgTime `shouldBe` tod

        it "handles midnight" do
          let tod = Time.TimeOfDay 0 0 0
              pgTime = PgTime.normalizeFromTimeOfDay tod
          PgTime.toTimeOfDay pgTime `shouldBe` tod

      describe "refineFromTimeOfDay" do
        it "accepts valid TimeOfDay" do
          let tod = Time.TimeOfDay 12 30 45
              result = PgTime.refineFromTimeOfDay tod
          result `shouldSatisfy` isJust
          fmap PgTime.toTimeOfDay result `shouldBe` Just tod

        it "accepts midnight" do
          let tod = Time.TimeOfDay 0 0 0
              result = PgTime.refineFromTimeOfDay tod
          result `shouldSatisfy` isJust

    describe "Accessors" do
      describe "toMicroseconds" do
        it "extracts microseconds value" do
          let pgTime = PgTime.fromMicroseconds 3661_000_000 -- 1 hour, 1 minute, 1 second
          PgTime.toMicroseconds pgTime `shouldBe` 3661_000_000

      describe "toTimeOfDay" do
        it "converts to TimeOfDay" do
          let pgTime = PgTime.fromMicroseconds 43_200_000_000 -- noon
              tod = PgTime.toTimeOfDay pgTime
          tod `shouldBe` Time.TimeOfDay 12 0 0

    describe "Property Tests" do
      it "roundtrips through toMicroseconds and fromMicroseconds" do
        property \(pgTime :: PgTime.Time) ->
          let micros = PgTime.toMicroseconds pgTime
              pgTime' = PgTime.fromMicroseconds micros
           in pgTime' === pgTime

      it "normalizeFromTimeOfDay is idempotent" do
        property \(tod :: Time.TimeOfDay) ->
          let normalized1 = PgTime.normalizeFromTimeOfDay tod
              normalized2 = PgTime.normalizeFromTimeOfDay (PgTime.toTimeOfDay normalized1)
           in normalized1 === normalized2
