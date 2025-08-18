-- | PostgreSQL @interval@ type.
-- Represents a time span in PostgreSQL with separate month, day, and microsecond components.
module PrimitiveLayer.Primitives.Interval (Interval) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PrimitiveLayer.Primitives.Interval.Micros as Micros
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @interval@ type with separate components for months, days, and microseconds.
data Interval = Interval
  { months :: Int32,
    days :: Int32,
    micros :: Int64
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Interval)

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
    days <- QuickCheck.choose (-30, 30)
    months <- QuickCheck.choose ((minBound @Interval).months, (maxBound @Interval).months)
    pure (max minBound (min maxBound (Interval {..})))

instance Primitive Interval where
  typeName = Tagged "interval"
  baseOid = Tagged 1186
  arrayOid = Tagged 1187
  binaryEncoder (Interval {..}) =
    mconcat [Write.bInt64 micros, Write.bInt32 days, Write.bInt32 months]
  binaryDecoder = PeekyBlinders.statically do
    micros <- PeekyBlinders.beSignedInt8
    days <- PeekyBlinders.beSignedInt4
    months <- PeekyBlinders.beSignedInt4
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

-- Note: Interval ↔ DiffTime conversion is not included because it would not be lawful.
-- Interval has separate month/day/microsecond components while DiffTime is a single duration.
-- Converting Interval → DiffTime → Interval would lose the month/day structure.

fromMicros :: Integer -> Interval
fromMicros =
  evalState do
    micros <- fromIntegral <$> state (swap . flip divMod Micros.day)
    days <- fromIntegral <$> state (swap . flip divMod 30)
    months <- fromIntegral <$> get
    pure Interval {..}

toMicros :: Interval -> Integer
toMicros Interval {..} =
  fromIntegral micros + 10 ^ 6 * 60 * 60 * 24 * (fromIntegral (days + 30 * months))

toPicos :: Interval -> Integer
toPicos = ((10 ^ 6) *) . toMicros

toDiffTime :: Interval -> DiffTime
toDiffTime = picosecondsToDiffTime . toPicos
