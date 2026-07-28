module PostgresqlTypes.TimestamptzSpec (spec) where

import Data.Data (Proxy (Proxy))
import qualified Data.Time as Time
import qualified PostgresqlTypes.Timestamptz as Timestamptz
import Test.Hspec
import Test.QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "Show/Read laws" do
    Scripts.testShowRead (Proxy @Timestamptz.Timestamptz)

  describe "IsPrimitive laws" do
    Scripts.testIsPrimitive (Proxy @Timestamptz.Timestamptz)

  describe "Constructors" do
    describe "normalizeFromUtcTime" do
      it "creates Timestamptz from UTCTime" do
        let day = Time.fromGregorian 2023 6 15
            diffTime = Time.secondsToDiffTime 45045 -- 12:30:45
            utcTime = Time.UTCTime day diffTime
            pgTimestamptz = Timestamptz.normalizeFromUtcTime utcTime
        Timestamptz.toUtcTime pgTimestamptz `shouldBe` utcTime

      it "handles epoch" do
        let day = Time.fromGregorian 2000 1 1
            diffTime = Time.secondsToDiffTime 0
            utcTime = Time.UTCTime day diffTime
            pgTimestamptz = Timestamptz.normalizeFromUtcTime utcTime
        Timestamptz.toUtcTime pgTimestamptz `shouldBe` utcTime

    describe "refineFromUtcTime" do
      it "accepts a UTCTime within range and precision" do
        let day = Time.fromGregorian 2023 6 15
            diffTime = Time.secondsToDiffTime 45045 -- 12:30:45
            utcTime = Time.UTCTime day diffTime
        fmap Timestamptz.toUtcTime (Timestamptz.refineFromUtcTime utcTime) `shouldBe` Just utcTime

      it "rejects a UTCTime with sub-microsecond precision" do
        let day = Time.fromGregorian 2023 6 15
            diffTime = Time.picosecondsToDiffTime (45045 * 1_000_000_000_000 + 123_456_500)
            utcTime = Time.UTCTime day diffTime
        Timestamptz.refineFromUtcTime utcTime `shouldBe` Nothing

  describe "Accessors" do
    describe "toUtcTime" do
      it "extracts UTCTime value" do
        let day = Time.fromGregorian 2023 6 15
            diffTime = Time.secondsToDiffTime 45045
            utcTime = Time.UTCTime day diffTime
            pgTimestamptz = Timestamptz.normalizeFromUtcTime utcTime
        Timestamptz.toUtcTime pgTimestamptz `shouldBe` utcTime

  describe "Property Tests" do
    it "roundtrips through toUtcTime and normalizeFromUtcTime" do
      property \(pgTimestamptz :: Timestamptz.Timestamptz) ->
        let utcTime = Timestamptz.toUtcTime pgTimestamptz
            pgTimestamptz' = Timestamptz.normalizeFromUtcTime utcTime
         in pgTimestamptz' === pgTimestamptz

    it "roundtrips through normalizeFromUtcTime and toUtcTime" do
      property \(pgTimestamptz :: Timestamptz.Timestamptz) ->
        let utcTime = Timestamptz.toUtcTime pgTimestamptz
            restored = Timestamptz.normalizeFromUtcTime utcTime
         in Timestamptz.toUtcTime restored === utcTime
