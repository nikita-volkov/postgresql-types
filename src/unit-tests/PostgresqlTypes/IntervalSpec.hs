module PostgresqlTypes.IntervalSpec (spec) where

import Data.Data (Proxy (Proxy))
import Data.Int
import Data.Maybe
import qualified Data.Time as Time
import qualified PostgresqlTypes.Interval as Interval
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck
import qualified UnitTests.Scripts as Scripts
import Prelude

spec :: Spec
spec = do
  describe "IsScalar" do
    Scripts.testIsScalar (Proxy @Interval.Interval)

  describe "Constructors" do
    describe "normalizeFromMonthsDaysAndMicroseconds" do
      it "clamps months values below minimum" do
        -- Use a value that's actually below minBound, not overflowed
        let minMonths = Interval.toMonths (minBound :: Interval.Interval)
            interval = Interval.normalizeFromMonthsDaysAndMicroseconds (minMonths - 1000) 0 0
        Interval.toMonths interval `shouldBe` minMonths

      it "clamps months values above maximum" do
        -- Use a value that's actually above maxBound, not overflowed
        let maxMonths = Interval.toMonths (maxBound :: Interval.Interval)
            interval = Interval.normalizeFromMonthsDaysAndMicroseconds (maxMonths + 1000) 0 0
        Interval.toMonths interval `shouldBe` maxMonths

      it "accepts valid values within range" do
        let interval = Interval.normalizeFromMonthsDaysAndMicroseconds 12 5 1_000_000
        Interval.toMonths interval `shouldBe` 12
        Interval.toDays interval `shouldBe` 5
        Interval.toMicroseconds interval `shouldBe` 1_000_000

    describe "refineFromMonthsDaysAndMicroseconds" do
      it "rejects months values below minimum" do
        let minMonths = Interval.toMonths (minBound :: Interval.Interval)
        Interval.refineFromMonthsDaysAndMicroseconds (minMonths - 1) 0 0 `shouldBe` Nothing

      it "rejects months values above maximum" do
        let maxMonths = Interval.toMonths (maxBound :: Interval.Interval)
        Interval.refineFromMonthsDaysAndMicroseconds (maxMonths + 1) 0 0 `shouldBe` Nothing

      it "accepts minimum months value" do
        let minMonths = Interval.toMonths (minBound :: Interval.Interval)
            result = Interval.refineFromMonthsDaysAndMicroseconds minMonths 0 0
        result `shouldSatisfy` isJust

      it "accepts maximum months value" do
        let maxMonths = Interval.toMonths (maxBound :: Interval.Interval)
            result = Interval.refineFromMonthsDaysAndMicroseconds maxMonths 0 0
        result `shouldSatisfy` isJust

      it "accepts valid values within range" do
        let result = Interval.refineFromMonthsDaysAndMicroseconds 12 5 1_000_000
        result `shouldSatisfy` isJust

      describe "normalizeFromMicrosecondsInTotal" do
        it "clamps values below minimum" do
          let minMicros = Interval.normalizeToMicrosecondsInTotal (minBound :: Interval.Interval)
              interval = Interval.normalizeFromMicrosecondsInTotal (minMicros - 1_000_000)
          interval `shouldBe` (minBound :: Interval.Interval)

        it "clamps values above maximum" do
          let maxMicros = Interval.normalizeToMicrosecondsInTotal (maxBound :: Interval.Interval)
              interval = Interval.normalizeFromMicrosecondsInTotal (maxMicros + 1_000_000)
          interval `shouldBe` (maxBound :: Interval.Interval)

        it "accepts valid values within range" do
          let microseconds = 1_000_000_000 -- ~11.5 days
              interval = Interval.normalizeFromMicrosecondsInTotal microseconds
          Interval.normalizeToMicrosecondsInTotal interval `shouldBe` microseconds

      describe "refineFromMicrosecondsInTotal" do
        it "rejects values below minimum" do
          let minMicros = Interval.normalizeToMicrosecondsInTotal (minBound :: Interval.Interval)
          Interval.refineFromMicrosecondsInTotal (minMicros - 1) `shouldBe` Nothing

        it "rejects values above maximum" do
          let maxMicros = Interval.normalizeToMicrosecondsInTotal (maxBound :: Interval.Interval)
          Interval.refineFromMicrosecondsInTotal (maxMicros + 1) `shouldBe` Nothing

        it "accepts minimum value" do
          let minMicros = Interval.normalizeToMicrosecondsInTotal (minBound :: Interval.Interval)
              result = Interval.refineFromMicrosecondsInTotal minMicros
          result `shouldSatisfy` isJust

        it "accepts maximum value" do
          let maxMicros = Interval.normalizeToMicrosecondsInTotal (maxBound :: Interval.Interval)
              result = Interval.refineFromMicrosecondsInTotal maxMicros
          result `shouldSatisfy` isJust

        it "accepts valid values within range" do
          let result = Interval.refineFromMicrosecondsInTotal 1_000_000_000
          result `shouldSatisfy` isJust

      describe "normalizeFromDiffTime" do
        it "clamps extreme negative DiffTime" do
          let minMicros = Interval.normalizeToMicrosecondsInTotal (minBound :: Interval.Interval)
              extremeDiffTime = Time.picosecondsToDiffTime ((minMicros - 1_000_000) * 1_000_000)
              interval = Interval.normalizeFromDiffTime extremeDiffTime
          interval `shouldBe` (minBound :: Interval.Interval)

        it "clamps extreme positive DiffTime" do
          let maxMicros = Interval.normalizeToMicrosecondsInTotal (maxBound :: Interval.Interval)
              extremeDiffTime = Time.picosecondsToDiffTime ((maxMicros + 1_000_000) * 1_000_000)
              interval = Interval.normalizeFromDiffTime extremeDiffTime
          interval `shouldBe` (maxBound :: Interval.Interval)

        it "loses sub-microsecond precision" do
          let diffTimeWithSubMicro = Time.picosecondsToDiffTime 1_000_500 -- 1.0005 microseconds
              interval = Interval.normalizeFromDiffTime diffTimeWithSubMicro
          -- Integer division truncates, so 1_000_500 / 1_000_000 = 1 microsecond
          Interval.normalizeToMicrosecondsInTotal interval `shouldBe` 1

      describe "refineFromDiffTime" do
        it "rejects DiffTime with sub-microsecond precision" do
          let diffTimeWithSubMicro = Time.picosecondsToDiffTime 1_000_500
          Interval.refineFromDiffTime diffTimeWithSubMicro `shouldBe` Nothing

        it "accepts DiffTime without sub-microsecond precision" do
          let diffTimeNoSubMicro = Time.picosecondsToDiffTime 1_000_000
              result = Interval.refineFromDiffTime diffTimeNoSubMicro
          result `shouldSatisfy` isJust

        it "rejects out of range DiffTime" do
          let maxMicros = Interval.normalizeToMicrosecondsInTotal (maxBound :: Interval.Interval)
              outOfRange = Time.picosecondsToDiffTime ((maxMicros + 1) * 1_000_000)
          Interval.refineFromDiffTime outOfRange `shouldBe` Nothing

    describe "Accessors" do
      describe "toMonths" do
        it "extracts months component" do
          let interval = Interval.normalizeFromMonthsDaysAndMicroseconds 12 0 0
          Interval.toMonths interval `shouldBe` 12

      describe "toDays" do
        it "extracts days component" do
          let interval = Interval.normalizeFromMonthsDaysAndMicroseconds 0 5 0
          Interval.toDays interval `shouldBe` 5

      describe "toMicroseconds" do
        it "extracts microseconds component" do
          let interval = Interval.normalizeFromMonthsDaysAndMicroseconds 0 0 1_000_000
          Interval.toMicroseconds interval `shouldBe` 1_000_000

      describe "normalizeToMicrosecondsInTotal" do
        it "converts interval to total microseconds" do
          let interval = Interval.normalizeFromMonthsDaysAndMicroseconds 1 1 1_000_000
              -- 1 month = 30 days, 1 day = 86400 seconds
              expectedMicros = (30 + 1) * 86400 * 1_000_000 + 1_000_000
          Interval.normalizeToMicrosecondsInTotal interval `shouldBe` expectedMicros

      describe "normalizeToDiffTime" do
        it "converts interval to DiffTime" do
          let interval = Interval.normalizeFromMonthsDaysAndMicroseconds 0 1 0
              diffTime = Interval.normalizeToDiffTime interval
              -- 1 day = 86400 seconds
              expectedPicos = 86400 * 1_000_000_000_000
          Time.diffTimeToPicoseconds diffTime `shouldBe` expectedPicos

    describe "Edge cases" do
      it "handles minimum bound interval" do
        let interval = minBound :: Interval.Interval
        -- Verify that minBound is actually set to the expected minimum months value
        Interval.toMonths interval `shouldBe` (-2136000000)

      it "handles maximum bound interval" do
        let interval = maxBound :: Interval.Interval
        -- Verify that maxBound is actually set to the expected maximum months value
        Interval.toMonths interval `shouldBe` 2136000000

      it "handles zero interval" do
        let interval = Interval.normalizeFromMonthsDaysAndMicroseconds 0 0 0
        Interval.toMonths interval `shouldBe` 0
        Interval.toDays interval `shouldBe` 0
        Interval.toMicroseconds interval `shouldBe` 0

    describe "Conversion instances" do
      describe "IsSome (Int32, Int32, Int64) Interval" do
        it "converts valid values" do
          let result = Interval.refineFromMonthsDaysAndMicroseconds 12 5 1_000_000
          result `shouldSatisfy` isJust

        it "rejects out-of-range values" do
          let maxMonths = Interval.toMonths (maxBound :: Interval.Interval)
              result = Interval.refineFromMonthsDaysAndMicroseconds (maxMonths + 1) 0 0
          result `shouldBe` Nothing

        it "roundtrips valid values" do
          QuickCheck.property \(interval :: Interval.Interval) ->
            let months = Interval.toMonths interval
                days = Interval.toDays interval
                microseconds = Interval.toMicroseconds interval
                restored = Interval.refineFromMonthsDaysAndMicroseconds months days microseconds
             in restored === Just interval

      describe "IsMany (Int32, Int32, Int64) Interval" do
        it "normalizes out-of-range values" do
          let maxMonths = Interval.toMonths (maxBound :: Interval.Interval)
              interval = Interval.normalizeFromMonthsDaysAndMicroseconds (maxMonths + 100) 0 0
          -- Value should be clamped
          Interval.toMonths interval `shouldBe` maxMonths

    describe "Property-based tests" do
      it "normalizeFrom* followed by refineFrom* is identity for components" do
        QuickCheck.property \(months :: Int32, days :: Int32, microseconds :: Int64) ->
          let normalized = Interval.normalizeFromMonthsDaysAndMicroseconds months days microseconds
              months' = Interval.toMonths normalized
              days' = Interval.toDays normalized
              microseconds' = Interval.toMicroseconds normalized
              projected = Interval.refineFromMonthsDaysAndMicroseconds months' days' microseconds'
           in projected === Just normalized

      it "normalizeToMicrosecondsInTotal followed by normalizeFromMicrosecondsInTotal preserves lossy conversion" do
        QuickCheck.property \(interval :: Interval.Interval) ->
          let microseconds = Interval.normalizeToMicrosecondsInTotal interval
              restored = Interval.normalizeFromMicrosecondsInTotal microseconds
           in Interval.normalizeToMicrosecondsInTotal restored === microseconds

      it "Eq is reflexive" do
        QuickCheck.property \(interval :: Interval.Interval) ->
          interval === interval

      it "Eq is symmetric" do
        QuickCheck.property \(i1 :: Interval.Interval, i2 :: Interval.Interval) ->
          (i1 == i2) === (i2 == i1)

      it "Ord is consistent with Eq" do
        QuickCheck.property \(i1 :: Interval.Interval, i2 :: Interval.Interval) ->
          (i1 == i2) === (compare i1 i2 == EQ)

      it "Ord is transitive" do
        QuickCheck.property \(i1 :: Interval.Interval, i2 :: Interval.Interval, i3 :: Interval.Interval) ->
          ((i1 <= i2) && (i2 <= i3)) ==> (i1 <= i3)

      it "Ord is total" do
        QuickCheck.property \(i1 :: Interval.Interval, i2 :: Interval.Interval) ->
          (i1 <= i2) .||. (i2 <= i1)

    describe "Boundary value tests" do
      it "handles microsecond precision" do
        let interval = Interval.normalizeFromMonthsDaysAndMicroseconds 0 0 1
        Interval.toMicroseconds interval `shouldBe` 1

      it "handles negative components" do
        let interval = Interval.normalizeFromMonthsDaysAndMicroseconds (-1) (-1) (-1)
        Interval.toMonths interval `shouldBe` (-1)
        Interval.toDays interval `shouldBe` (-1)
        Interval.toMicroseconds interval `shouldBe` (-1)
