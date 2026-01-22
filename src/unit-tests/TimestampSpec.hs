module TimestampSpec (spec) where

import qualified Data.Time as Time
import qualified PostgresqlTypes.Timestamp as Timestamp
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Timestamp" do
    describe "Constructors" do
      describe "fromLocalTime" do
        it "creates Timestamp from LocalTime" do
          let day = Time.fromGregorian 2023 6 15
              tod = Time.TimeOfDay 12 30 45
              localTime = Time.LocalTime day tod
              pgTimestamp = Timestamp.fromLocalTime localTime
          Timestamp.toLocalTime pgTimestamp `shouldBe` localTime

        it "handles epoch" do
          let day = Time.fromGregorian 2000 1 1
              tod = Time.TimeOfDay 0 0 0
              localTime = Time.LocalTime day tod
              pgTimestamp = Timestamp.fromLocalTime localTime
          Timestamp.toLocalTime pgTimestamp `shouldBe` localTime

    describe "Accessors" do
      describe "toLocalTime" do
        it "extracts LocalTime value" do
          let day = Time.fromGregorian 2023 6 15
              tod = Time.TimeOfDay 12 30 45
              localTime = Time.LocalTime day tod
              pgTimestamp = Timestamp.fromLocalTime localTime
          Timestamp.toLocalTime pgTimestamp `shouldBe` localTime

    describe "Property Tests" do
      it "roundtrips through toLocalTime and fromLocalTime" do
        property \(pgTimestamp :: Timestamp.Timestamp) ->
          let localTime = Timestamp.toLocalTime pgTimestamp
              pgTimestamp' = Timestamp.fromLocalTime localTime
           in pgTimestamp' === pgTimestamp

      it "roundtrips through fromLocalTime and toLocalTime for valid LocalTimes" do
        property \(pgTimestamp :: Timestamp.Timestamp) ->
          let localTime = Timestamp.toLocalTime pgTimestamp
              restored = Timestamp.fromLocalTime localTime
           in Timestamp.toLocalTime restored === localTime
