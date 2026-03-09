module PostgresqlTypes.TimeSpec (spec) where

import qualified Data.Attoparsec.Text
import Data.Data (Proxy (Proxy))
import Data.Either (isLeft, isRight)
import Data.Maybe
import qualified Data.Time as Time
import qualified PostgresqlTypes.Algebra
import qualified PostgresqlTypes.Time as PgTime
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @PgTime.Time)

  describe "IsScalar laws" do
    Scripts.testIsScalar (Proxy @PgTime.Time)

  describe "Constructors" do
    describe "normalizeFromMicroseconds" do
      it "creates Time from microseconds" do
        let pgTime = PgTime.normalizeFromMicroseconds 43_200_000_000 -- noon
        PgTime.toMicroseconds pgTime `shouldBe` 43_200_000_000

      it "creates Time from zero (midnight)" do
        let pgTime = PgTime.normalizeFromMicroseconds 0
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
        let pgTime = PgTime.normalizeFromMicroseconds 3661_000_000 -- 1 hour, 1 minute, 1 second
        PgTime.toMicroseconds pgTime `shouldBe` 3661_000_000

    describe "toTimeOfDay" do
      it "converts to TimeOfDay" do
        let pgTime = PgTime.normalizeFromMicroseconds 43_200_000_000 -- noon
            tod = PgTime.toTimeOfDay pgTime
        tod `shouldBe` Time.TimeOfDay 12 0 0

  describe "Edge Cases" do
    it "textual decoder accepts 24:00:00" do
      let result = Data.Attoparsec.Text.parseOnly (PostgresqlTypes.Algebra.textualDecoder @PgTime.Time) "24:00:00"
      result `shouldSatisfy` isRight
      fmap PgTime.toMicroseconds result `shouldBe` Right 86_400_000_000

    it "textual decoder rejects 24:00:01" do
      let result = Data.Attoparsec.Text.parseOnly (PostgresqlTypes.Algebra.textualDecoder @PgTime.Time) "24:00:01"
      result `shouldSatisfy` isLeft

    it "textual decoder rejects 24:01:00" do
      let result = Data.Attoparsec.Text.parseOnly (PostgresqlTypes.Algebra.textualDecoder @PgTime.Time) "24:01:00"
      result `shouldSatisfy` isLeft

    it "textual decoder rejects 24:30:00" do
      let result = Data.Attoparsec.Text.parseOnly (PostgresqlTypes.Algebra.textualDecoder @PgTime.Time) "24:30:00"
      result `shouldSatisfy` isLeft

    it "textual decoder rejects 24:00:00.000001" do
      let result = Data.Attoparsec.Text.parseOnly (PostgresqlTypes.Algebra.textualDecoder @PgTime.Time) "24:00:00.000001"
      result `shouldSatisfy` isLeft

    it "textual decoder accepts 23:59:59.999999" do
      let result = Data.Attoparsec.Text.parseOnly (PostgresqlTypes.Algebra.textualDecoder @PgTime.Time) "23:59:59.999999"
      result `shouldSatisfy` isRight

  describe "Property Tests" do
    it "roundtrips through toMicroseconds and normalizeFromMicroseconds" do
      property \(pgTime :: PgTime.Time) ->
        let micros = PgTime.toMicroseconds pgTime
            pgTime' = PgTime.normalizeFromMicroseconds micros
         in pgTime' === pgTime

    it "normalizeFromTimeOfDay is idempotent" do
      property \(tod :: Time.TimeOfDay) ->
        let normalized1 = PgTime.normalizeFromTimeOfDay tod
            normalized2 = PgTime.normalizeFromTimeOfDay (PgTime.toTimeOfDay normalized1)
         in normalized1 === normalized2
