module PrimitiveLayer.Types.Timestamptz (Timestamptz) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @timestamptz@ type. Date and time with time zone.
--
-- Gets stored as microseconds since PostgreSQL epoch, implying UTC timezone.
--
-- Range: @4713 BC@ to @294276 AD@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-TIMEZONES).
newtype Timestamptz = Timestamptz Int64
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Timestamptz)

instance Arbitrary Timestamptz where
  arbitrary = Timestamptz <$> QuickCheck.choose (pgTimestampMin, pgTimestampMax)
    where
      -- PostgreSQL's actual documented timestamptz range: 4713 BC to 294276 AD
      -- Do not artificially restrict to avoid edge cases - let the tests expose real issues
      pgTimestampMin = -210866803200000000 -- 4713 BC January 1 00:00:00 UTC
      pgTimestampMax = 9214646400000000000 -- 294276 AD December 31 23:59:59.999999 UTC

instance Mapping Timestamptz where
  typeName = Tagged "timestamptz"
  baseOid = Tagged 1184
  arrayOid = Tagged 1185
  binaryEncoder (Timestamptz micros) = Write.bInt64 micros
  binaryDecoder = do
    microseconds <- PeekyBlinders.statically PeekyBlinders.beSignedInt8
    pure (Right (Timestamptz microseconds))
  textualEncoder (toUtcTime -> utcTime) =
    TextBuilder.string (formatTimestamptzForPostgreSQL utcTime)

-- | Mapping to @tstzrange@ type.
instance RangeMapping Timestamptz where
  rangeTypeName = Tagged "tstzrange"
  rangeOid = Tagged 3910
  rangeArrayOid = Tagged 3911

-- | Mapping to @tstzmultirange@ type.
instance MultirangeMapping Timestamptz where
  multirangeTypeName = Tagged "tstzmultirange"
  multirangeOid = Tagged 4534
  multirangeArrayOid = Tagged 6153

-- PostgreSQL timestamptz epoch is 2000-01-01 00:00:00 UTC
postgresUtcEpoch :: Time.UTCTime
postgresUtcEpoch = Time.UTCTime (Time.fromGregorian 2000 1 1) 0

toUtcTime :: Timestamptz -> Time.UTCTime
toUtcTime (Timestamptz micros) =
  let diffTime = fromIntegral micros / 1_000_000
   in Time.addUTCTime diffTime postgresUtcEpoch

fromUtcTime :: Time.UTCTime -> Timestamptz
fromUtcTime utcTime =
  let diffTime = Time.diffUTCTime utcTime postgresUtcEpoch
      micros = round (diffTime * 1_000_000)
   in Timestamptz micros

-- | Direct conversion from 'Data.Time.UTCTime'.
-- This is always safe since both represent UTC timestamps.
instance IsSome Time.UTCTime Timestamptz where
  to = toUtcTime
  maybeFrom utcTime =
    let diffTime = Time.diffUTCTime utcTime postgresUtcEpoch
        picoSeconds = Time.nominalDiffTimeToSeconds diffTime
        -- Convert Pico to Integer picoseconds (Pico has 10^12 precision)
        picosecondsInteger = round (toRational picoSeconds * 1_000_000_000_000)
        (microseconds, remainder) = divMod picosecondsInteger 1_000_000
     in if remainder == 0
          then Just (Timestamptz (fromIntegral microseconds))
          else Nothing

-- | Direct conversion from 'Data.Time.UTCTime'.
-- This is a total conversion as it always succeeds.
instance IsMany Time.UTCTime Timestamptz where
  onfrom = fromUtcTime

-- | Format a UTCTime for PostgreSQL timestamptz text format.
-- PostgreSQL requires specific formatting for extreme dates:
-- - Years must be 4-digit zero-padded for AD dates
-- - BC dates use "YYYY-MM-DD HH:MM:SS+0000 BC" format
-- - Always includes +0000 timezone for UTC
-- - Microseconds must be properly formatted
formatTimestamptzForPostgreSQL :: Time.UTCTime -> String
formatTimestamptzForPostgreSQL utcTime =
  let day = Time.utctDay utcTime
      diffTime = Time.utctDayTime utcTime
      (year, month, dayOfMonth) = Time.toGregorian day
      -- Convert DiffTime to hours, minutes, seconds, microseconds
      totalSecs = floor (toRational diffTime) :: Integer
      totalMicros = round (toRational diffTime * 1000000) :: Integer
      micros = totalMicros `mod` 1000000 :: Integer
      secs = totalSecs `mod` 60 :: Integer
      totalMins = totalSecs `div` 60 :: Integer
      mins = totalMins `mod` 60 :: Integer
      hours = totalMins `div` 60 :: Integer
   in if year <= 0
        then
          -- For BC dates (year <= 0), PostgreSQL expects format like "0001-01-01 00:00:00+0000 BC"
          -- Note: year 0 is 1 BC, year -1 is 2 BC, etc.
          let bcYear = 1 - year
           in if micros == 0
                then printf "%04d-%02d-%02d %02d:%02d:%02d+0000 BC" (bcYear :: Integer) (month :: Int) (dayOfMonth :: Int) hours mins secs
                else printf "%04d-%02d-%02d %02d:%02d:%02d.%06d+0000 BC" (bcYear :: Integer) (month :: Int) (dayOfMonth :: Int) hours mins secs micros
        else
          -- For AD dates (year > 0), use zero-padded 4-digit year
          if micros == 0
            then printf "%04d-%02d-%02d %02d:%02d:%02d+0000" (year :: Integer) (month :: Int) (dayOfMonth :: Int) hours mins secs
            else printf "%04d-%02d-%02d %02d:%02d:%02d.%06d+0000" (year :: Integer) (month :: Int) (dayOfMonth :: Int) hours mins secs micros
