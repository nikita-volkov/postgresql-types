module PostgresqlTypes.TimestamptzSpec (spec) where

import qualified Data.Time as Time
import qualified PostgresqlTypes.Timestamptz as Timestamptz
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Timestamptz" do
    describe "Constructors" do
      describe "fromUtcTime" do
        it "creates Timestamptz from UTCTime" do
          let day = Time.fromGregorian 2023 6 15
              diffTime = Time.secondsToDiffTime 45045 -- 12:30:45
              utcTime = Time.UTCTime day diffTime
              pgTimestamptz = Timestamptz.fromUtcTime utcTime
          Timestamptz.toUtcTime pgTimestamptz `shouldBe` utcTime

        it "handles epoch" do
          let day = Time.fromGregorian 2000 1 1
              diffTime = Time.secondsToDiffTime 0
              utcTime = Time.UTCTime day diffTime
              pgTimestamptz = Timestamptz.fromUtcTime utcTime
          Timestamptz.toUtcTime pgTimestamptz `shouldBe` utcTime

    describe "Accessors" do
      describe "toUtcTime" do
        it "extracts UTCTime value" do
          let day = Time.fromGregorian 2023 6 15
              diffTime = Time.secondsToDiffTime 45045
              utcTime = Time.UTCTime day diffTime
              pgTimestamptz = Timestamptz.fromUtcTime utcTime
          Timestamptz.toUtcTime pgTimestamptz `shouldBe` utcTime

    describe "Property Tests" do
      it "roundtrips through toUtcTime and fromUtcTime" do
        property \(pgTimestamptz :: Timestamptz.Timestamptz) ->
          let utcTime = Timestamptz.toUtcTime pgTimestamptz
              pgTimestamptz' = Timestamptz.fromUtcTime utcTime
           in pgTimestamptz' === pgTimestamptz

      it "roundtrips through fromUtcTime and toUtcTime" do
        property \(pgTimestamptz :: Timestamptz.Timestamptz) ->
          let utcTime = Timestamptz.toUtcTime pgTimestamptz
              restored = Timestamptz.fromUtcTime utcTime
           in Timestamptz.toUtcTime restored === utcTime
