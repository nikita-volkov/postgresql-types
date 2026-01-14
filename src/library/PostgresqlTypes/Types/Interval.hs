{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlTypes.Types.Interval (Interval) where

import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @interval@ type. Time span with separate components for months, days, and microseconds with individual signs.
--
-- For a simpler and more portable representation consider 'PostgresqlTypes.Types.IntervalAsMicroseconds.IntervalAsMicroseconds'.
--
-- Range: @-178000000@ years to @178000000@ years.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-INTERVAL-INPUT).
data Interval = Interval
  { months :: Int32,
    days :: Int32,
    micros :: Int64
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsPrimitive Interval)

instance Bounded Interval where
  minBound =
    Interval
      { months = -178000000 * 12,
        days = 0,
        micros = 0
      }
  maxBound =
    Interval
      { months = 178000000 * 12,
        days = 0,
        micros = 0
      }

instance Arbitrary Interval where
  arbitrary = do
    micros <- QuickCheck.choose (-999_999, 999_999)
    days <- QuickCheck.choose (-daysPerMonth, daysPerMonth)
    months <- QuickCheck.choose ((minBound @Interval).months, (maxBound @Interval).months)
    pure (max minBound (min maxBound (Interval {..})))

instance IsPrimitive Interval where
  typeName = Tagged "interval"
  baseOid = Tagged 1186
  arrayOid = Tagged 1187
  binaryEncoder (Interval {..}) =
    mconcat [Write.bInt64 micros, Write.bInt32 days, Write.bInt32 months]
  binaryDecoder = PtrPeeker.fixed do
    micros <- PtrPeeker.beSignedInt8
    days <- PtrPeeker.beSignedInt4
    months <- PtrPeeker.beSignedInt4
    pure (Right (Interval {..}))

  -- Renders in "format with designators" of ISO-8601 as per [the Postgres documentation](https://www.postgresql.org/docs/current/datatype-datetime.html#DATATYPE-INTERVAL-INPUT).
  --
  -- >P quantity unit [ quantity unit ...] [ T [ quantity unit ...]]
  textualEncoder (Interval {..}) =
    let monthsSign = if months < 0 then "-" else ""
        daysSign = if days < 0 then "-" else ""
        microsSign = if micros < 0 then "-" else ""
        absMonths = abs months
        absDays = abs days
        yearsPart =
          if absMonths >= 12
            then monthsSign <> TextBuilder.decimal (absMonths `div` 12) <> "Y"
            else mempty
        monthsPart =
          if absMonths `mod` 12 /= 0
            then monthsSign <> TextBuilder.decimal (absMonths `mod` 12) <> "M"
            else mempty
        daysPart =
          if absDays /= 0
            then daysSign <> TextBuilder.decimal absDays <> "D"
            else mempty

        totalMicros = abs micros
        hours = totalMicros `div` (60 * 60 * 1000000)
        remainingMicros = totalMicros `mod` (60 * 60 * 1000000)
        minutes = remainingMicros `div` (60 * 1000000)
        seconds = remainingMicros `mod` (60 * 1000000)

        hoursPart =
          if hours /= 0
            then microsSign <> TextBuilder.decimal hours <> "H"
            else mempty
        minutesPart =
          if minutes /= 0
            then microsSign <> TextBuilder.decimal minutes <> "M"
            else mempty
        secondsPart =
          if seconds /= 0
            then
              microsSign
                <> if seconds `mod` 1000000 == 0
                  then TextBuilder.decimal (seconds `div` 1000000) <> "S"
                  else
                    TextBuilder.decimal (seconds `div` 1000000)
                      <> "."
                      <> TextBuilder.fixedLengthDecimal 6 (seconds `mod` 1000000)
                      <> "S"
            else mempty

        timePart = hoursPart <> minutesPart <> secondsPart
        tPrefix = if timePart /= mempty then "T" else mempty
        datePart = yearsPart <> monthsPart <> daysPart
     in if datePart == mempty && timePart == mempty
          then "PT0S"
          else "P" <> datePart <> tPrefix <> timePart

-- | Safe conversion from tuple representation (months, days, microseconds) to Interval.
-- Validates that the input values are within PostgreSQL's valid range for intervals.
instance IsSome (Int32, Int32, Int64) Interval where
  to (Interval {..}) = (months, days, micros)
  maybeFrom (months, days, micros) =
    let interval = Interval {..}
     in if interval >= minBound && interval <= maxBound
          then Just interval
          else Nothing

-- | Total conversion from tuple representation (months, days, microseconds) to Interval.
-- Preserves the structured representation while clamping to valid ranges.
instance IsMany (Int32, Int32, Int64) Interval where
  onfrom (months, days, micros) =
    let interval = Interval {..}
     in -- First try the direct interval, then clamp to bounds if needed
        if interval >= minBound && interval <= maxBound
          then interval
          else max minBound (min maxBound interval)

fromMicros :: Integer -> Interval
fromMicros =
  evalState do
    micros <- fromIntegral <$> state (swap . flip divMod microsPerDay)
    days <- fromIntegral <$> state (swap . flip divMod daysPerMonth)
    months <- fromIntegral <$> get
    pure Interval {..}

toMicros :: Interval -> Integer
toMicros Interval {..} =
  fromIntegral micros + microsPerDay * (fromIntegral days + daysPerMonth * fromIntegral months)

toPicos :: Interval -> Integer
toPicos = (1_000_000 *) . toMicros

toDiffTime :: Interval -> DiffTime
toDiffTime = picosecondsToDiffTime . toPicos

microsPerDay :: (Num a) => a
microsPerDay = 1_000_000 * 60 * 60 * 24

daysPerMonth :: (Num a) => a
daysPerMonth = 30
