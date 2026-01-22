module PostgresqlTypes.TimetzSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Int
import Data.Maybe
import qualified Data.Time as Time
import qualified LawfulConversions
import qualified PostgresqlTypes.Timetz as Timetz
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Timetz.Timetz)

  describe "Constructors" do
    describe "normalizeFromTimeInMicrosecondsAndOffsetInSeconds" do
      it "clamps time values below minimum" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds (-100) 0
        Timetz.toTimeInMicroseconds timetz `shouldBe` 0

      it "clamps time values above maximum" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 90_000_000_000 0
        Timetz.toTimeInMicroseconds timetz `shouldBe` 86_400_000_000

      it "clamps offset values below minimum" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 (-60_000)
        Timetz.toTimeZoneInSeconds timetz `shouldBe` (-57_599)

      it "clamps offset values above maximum" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 60_000
        Timetz.toTimeZoneInSeconds timetz `shouldBe` 57_599

      it "accepts valid values within range" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 43_200_000_000 3600
        Timetz.toTimeInMicroseconds timetz `shouldBe` 43_200_000_000
        Timetz.toTimeZoneInSeconds timetz `shouldBe` 3600

    describe "refineFromTimeInMicrosecondsAndOffsetInSeconds" do
      it "rejects time values below minimum" do
        Timetz.refineFromTimeInMicrosecondsAndOffsetInSeconds (-1) 0 `shouldBe` Nothing

      it "rejects time values above maximum" do
        Timetz.refineFromTimeInMicrosecondsAndOffsetInSeconds 86_400_000_001 0 `shouldBe` Nothing

      it "rejects offset values below minimum" do
        Timetz.refineFromTimeInMicrosecondsAndOffsetInSeconds 0 (-57_600) `shouldBe` Nothing

      it "rejects offset values above maximum" do
        Timetz.refineFromTimeInMicrosecondsAndOffsetInSeconds 0 57_600 `shouldBe` Nothing

      it "accepts minimum time and offset" do
        let result = Timetz.refineFromTimeInMicrosecondsAndOffsetInSeconds 0 (-57_599)
        result `shouldSatisfy` isJust

      it "accepts maximum time and offset" do
        let result = Timetz.refineFromTimeInMicrosecondsAndOffsetInSeconds 86_400_000_000 57_599
        result `shouldSatisfy` isJust

      it "accepts valid values within range" do
        let result = Timetz.refineFromTimeInMicrosecondsAndOffsetInSeconds 43_200_000_000 3600
        result `shouldSatisfy` isJust

      describe "normalizeFromTimeOfDayAndTimeZone" do
        it "clamps invalid TimeOfDay to valid range" do
          let invalidTimeOfDay = Time.TimeOfDay 25 0 0 -- Invalid hour
          -- This will be normalized internally by TimeOfDay
              timetz = Timetz.normalizeFromTimeOfDayAndTimeZone invalidTimeOfDay (Time.TimeZone 0 False "UTC")
          -- The time should be normalized (wrapped around)
          Timetz.toTimeInMicroseconds timetz `shouldSatisfy` (<= 86_400_000_000)

        it "clamps extreme time zone offsets" do
          let timeOfDay = Time.TimeOfDay 12 0 0
              extremeOffset = Time.TimeZone 10000 False "EXTREME" -- Very large offset
              timetz = Timetz.normalizeFromTimeOfDayAndTimeZone timeOfDay extremeOffset
          -- Offset should be clamped to valid range
          abs (Timetz.toTimeZoneInSeconds timetz) `shouldSatisfy` (<= 57_599)

        it "handles valid time and time zone" do
          let timeOfDay = Time.TimeOfDay 15 30 45
              timeZone = Time.TimeZone 60 False "UTC+1" -- +1 hour
              timetz = Timetz.normalizeFromTimeOfDayAndTimeZone timeOfDay timeZone
          Timetz.toTimeInMicroseconds timetz `shouldBe` (15 * 3600 + 30 * 60 + 45) * 1_000_000
          Timetz.toTimeZoneInSeconds timetz `shouldBe` 3600

      describe "refineFromTimeOfDayAndTimeZone" do
        it "rejects TimeOfDay that doesn't fit in valid range" do
          -- TimeOfDay with 24:00:00.000001 would exceed maximum
          let result = Timetz.refineFromTimeOfDayAndTimeZone (Time.TimeOfDay 24 0 0.000001) (Time.TimeZone 0 False "UTC")
          result `shouldBe` Nothing

        it "accepts midnight (00:00:00)" do
          let result = Timetz.refineFromTimeOfDayAndTimeZone (Time.TimeOfDay 0 0 0) (Time.TimeZone 0 False "UTC")
          result `shouldSatisfy` isJust

        it "accepts exactly 24:00:00" do
          let result = Timetz.refineFromTimeOfDayAndTimeZone (Time.TimeOfDay 24 0 0) (Time.TimeZone 0 False "UTC")
          result `shouldSatisfy` isJust

        it "accepts valid time and time zone" do
          let result = Timetz.refineFromTimeOfDayAndTimeZone (Time.TimeOfDay 15 30 45) (Time.TimeZone 60 False "UTC+1")
          result `shouldSatisfy` isJust

    describe "Accessors" do
      describe "toTimeInMicroseconds" do
        it "extracts time in microseconds" do
          let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 43_200_000_000 0
          Timetz.toTimeInMicroseconds timetz `shouldBe` 43_200_000_000

      describe "toTimeZoneInSeconds" do
        it "extracts time zone offset in seconds" do
          let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 3600
          Timetz.toTimeZoneInSeconds timetz `shouldBe` 3600

      describe "toTimeOfDay" do
        it "converts to TimeOfDay correctly" do
          let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds ((15 * 3600 + 30 * 60 + 45) * 1_000_000) 0
              timeOfDay = Timetz.toTimeOfDay timetz
          timeOfDay `shouldBe` Time.TimeOfDay 15 30 45

      describe "normalizeToTimeZone" do
        it "normalizes offset to TimeZone with minute precision" do
          -- Test with offset that is a multiple of 60
          let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 3600
              timeZone = Timetz.normalizeToTimeZone timetz
          Time.timeZoneMinutes timeZone `shouldBe` 60

        it "rounds offset that is not a multiple of 60" do
          -- Test with offset that includes seconds
          let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 3630 -- 60:30
              timeZone = Timetz.normalizeToTimeZone timetz
          -- Should be rounded to nearest minute
          Time.timeZoneMinutes timeZone `shouldSatisfy` (\m -> abs (m - 60) <= 1)

      describe "refineToTimeZone" do
        it "returns Just for offset that is a multiple of 60" do
          let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 3600
          Timetz.refineToTimeZone timetz `shouldSatisfy` isJust

        it "returns Nothing for offset that is not a multiple of 60" do
          let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 3630
          Timetz.refineToTimeZone timetz `shouldBe` Nothing

    describe "Edge cases" do
      it "handles minimum bound time (00:00:00)" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 0
        Timetz.toTimeOfDay timetz `shouldBe` Time.TimeOfDay 0 0 0

      it "handles maximum bound time (24:00:00)" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 86_400_000_000 0
        Timetz.toTimeOfDay timetz `shouldBe` Time.TimeOfDay 24 0 0

      it "handles minimum time zone offset (-15:59:59)" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 (-57_599)
        Timetz.toTimeZoneInSeconds timetz `shouldBe` (-57_599)

      it "handles maximum time zone offset (+15:59:59)" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 57_599
        Timetz.toTimeZoneInSeconds timetz `shouldBe` 57_599

      it "handles midnight UTC" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 0
        Timetz.toTimeInMicroseconds timetz `shouldBe` 0
        Timetz.toTimeZoneInSeconds timetz `shouldBe` 0

      it "handles noon with positive offset" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds (12 * 3600 * 1_000_000) 3600
        Timetz.toTimeOfDay timetz `shouldBe` Time.TimeOfDay 12 0 0
        Timetz.toTimeZoneInSeconds timetz `shouldBe` 3600

      it "handles noon with negative offset" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds (12 * 3600 * 1_000_000) (-3600)
        Timetz.toTimeOfDay timetz `shouldBe` Time.TimeOfDay 12 0 0
        Timetz.toTimeZoneInSeconds timetz `shouldBe` (-3600)

    describe "Conversion instances" do
      describe "IsSome (Int64, Int32) Timetz" do
        it "converts valid values" do
          let tuple = (43_200_000_000, 3600)
              result = LawfulConversions.maybeFrom @(Int64, Int32) @(Timetz.Timetz) tuple
          result `shouldSatisfy` isJust

        it "rejects invalid time values" do
          let tuple = (90_000_000_000, 0)
              result = LawfulConversions.maybeFrom @(Int64, Int32) @(Timetz.Timetz) tuple
          result `shouldBe` Nothing

        it "rejects invalid offset values" do
          let tuple = (0, 60_000)
              result = LawfulConversions.maybeFrom @(Int64, Int32) @(Timetz.Timetz) tuple
          result `shouldBe` Nothing

        it "roundtrips valid values" do
          QuickCheck.property \(timetz :: Timetz.Timetz) ->
            let tuple = LawfulConversions.to @(Int64, Int32) timetz
                restored = LawfulConversions.maybeFrom @(Int64, Int32) @(Timetz.Timetz) tuple
             in restored === Just timetz

      describe "IsMany (Int64, Int32) Timetz" do
        it "normalizes out-of-range values" do
          let tuple = (90_000_000_000, 60_000)
              timetz = LawfulConversions.onfrom @(Int64, Int32) @(Timetz.Timetz) tuple
          -- Values should be clamped
          Timetz.toTimeInMicroseconds timetz `shouldBe` 86_400_000_000
          Timetz.toTimeZoneInSeconds timetz `shouldBe` 57_599

    describe "Property-based tests" do
      it "toTimeInMicroseconds returns value in valid range" do
        QuickCheck.property \(timetz :: Timetz.Timetz) ->
          let microseconds = Timetz.toTimeInMicroseconds timetz
           in microseconds >= 0 .&&. microseconds <= 86_400_000_000

      it "toTimeZoneInSeconds returns value in valid range" do
        QuickCheck.property \(timetz :: Timetz.Timetz) ->
          let seconds = Timetz.toTimeZoneInSeconds timetz
           in seconds >= (-57_599) .&&. seconds <= 57_599

      it "normalizeFrom* followed by refineFrom* is identity" do
        QuickCheck.property \(microseconds :: Int64, seconds :: Int32) ->
          let normalized = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds microseconds seconds
              microseconds' = Timetz.toTimeInMicroseconds normalized
              seconds' = Timetz.toTimeZoneInSeconds normalized
              projected = Timetz.refineFromTimeInMicrosecondsAndOffsetInSeconds microseconds' seconds'
           in projected === Just normalized

      it "toTimeOfDay followed by normalizeFromTimeOfDay preserves value" do
        QuickCheck.property \(timetz :: Timetz.Timetz) ->
          let timeOfDay = Timetz.toTimeOfDay timetz
              timeZone = Timetz.normalizeToTimeZone timetz
              restored = Timetz.normalizeFromTimeOfDayAndTimeZone timeOfDay timeZone
           in Timetz.toTimeInMicroseconds restored === Timetz.toTimeInMicroseconds timetz

      it "Eq is reflexive" do
        QuickCheck.property \(timetz :: Timetz.Timetz) ->
          timetz === timetz

      it "Eq is symmetric" do
        QuickCheck.property \(t1 :: Timetz.Timetz, t2 :: Timetz.Timetz) ->
          (t1 == t2) === (t2 == t1)

      it "Eq is transitive" do
        QuickCheck.property \(t1 :: Timetz.Timetz, t2 :: Timetz.Timetz, t3 :: Timetz.Timetz) ->
          if (t1 == t2) && (t2 == t3)
            then t1 === t3
            else property True

      it "Ord is consistent with Eq" do
        QuickCheck.property \(t1 :: Timetz.Timetz, t2 :: Timetz.Timetz) ->
          (t1 == t2) === (compare t1 t2 == EQ)

      it "Ord is transitive" do
        QuickCheck.property \(t1 :: Timetz.Timetz, t2 :: Timetz.Timetz, t3 :: Timetz.Timetz) ->
          ((t1 <= t2) && (t2 <= t3)) ==> (t1 <= t3)

      it "Ord is antisymmetric" do
        QuickCheck.property \(t1 :: Timetz.Timetz, t2 :: Timetz.Timetz) ->
          if (t1 <= t2) && (t2 <= t1)
            then t1 === t2
            else property True

      it "Ord is total" do
        QuickCheck.property \(t1 :: Timetz.Timetz, t2 :: Timetz.Timetz) ->
          (t1 <= t2) .||. (t2 <= t1)

    describe "Boundary value tests" do
      it "handles microsecond precision" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 1 0
        Timetz.toTimeInMicroseconds timetz `shouldBe` 1

      it "handles maximum microseconds (one microsecond before 24:00:00)" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds (86_400_000_000 - 1) 0
        Timetz.toTimeInMicroseconds timetz `shouldBe` (86_400_000_000 - 1)

      it "handles one second offset" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 1
        Timetz.toTimeZoneInSeconds timetz `shouldBe` 1

      it "handles one second before minimum offset" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 (-57_598)
        Timetz.toTimeZoneInSeconds timetz `shouldBe` (-57_598)

      it "handles one second before maximum offset" do
        let timetz = Timetz.normalizeFromTimeInMicrosecondsAndOffsetInSeconds 0 57_598
        Timetz.toTimeZoneInSeconds timetz `shouldBe` 57_598
