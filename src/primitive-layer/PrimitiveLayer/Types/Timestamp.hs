module PrimitiveLayer.Types.Timestamp (Timestamp) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @timestamp@ type. Date and time (without time zone).
--
-- Gets stored as microseconds since PostgreSQL epoch.
--
-- Range: @4713 BC@ to @294276 AD@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-DATETIME).
newtype Timestamp = Timestamp Int64
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Timestamp)

instance Arbitrary Timestamp where
  arbitrary = Timestamp <$> QuickCheck.choose (pgTimestampMin, pgTimestampMax)
    where
      -- PostgreSQL's actual documented timestamp range: 4713 BC to 294276 AD
      -- Do not artificially restrict to avoid edge cases - let the tests expose real issues
      pgTimestampMin = -210866803200000000 -- 4713 BC January 1 00:00:00
      pgTimestampMax = 9214646400000000000 -- 294276 AD December 31 23:59:59.999999

instance Mapping Timestamp where
  typeName = Tagged "timestamp"
  baseOid = Tagged 1114
  arrayOid = Tagged 1115
  binaryEncoder (Timestamp micros) = Write.bInt64 micros
  binaryDecoder = do
    microseconds <- PeekyBlinders.statically PeekyBlinders.beSignedInt8
    pure (Right (Timestamp microseconds))
  textualEncoder (toLocalTime -> localTime) =
    TextBuilder.string (formatTimestampForPostgreSQL localTime)

-- | Mapping to @tsrange@ type.
instance RangeMapping Timestamp where
  rangeTypeName = Tagged "tsrange"
  rangeOid = Tagged 3908
  rangeArrayOid = Tagged 3909

-- | Mapping to @tsmultirange@ type.
instance MultirangeMapping Timestamp where
  multirangeTypeName = Tagged "tsmultirange"
  multirangeOid = Tagged 4533
  multirangeArrayOid = Tagged 6152

-- PostgreSQL timestamp epoch is 2000-01-01 00:00:00
postgresTimestampEpoch :: Time.LocalTime
postgresTimestampEpoch = Time.LocalTime (Time.fromGregorian 2000 1 1) Time.midnight

toLocalTime :: Timestamp -> Time.LocalTime
toLocalTime (Timestamp micros) =
  let diffTime = fromIntegral micros / 1_000_000
   in Time.addLocalTime diffTime postgresTimestampEpoch

fromLocalTime :: Time.LocalTime -> Timestamp
fromLocalTime localTime =
  let diffTime = Time.diffLocalTime localTime postgresTimestampEpoch
      micros = round (diffTime * 1_000_000)
   in Timestamp micros

-- | Direct conversion from 'Data.Time.LocalTime'.
-- This is always safe since both represent local timestamps.
instance IsSome Time.LocalTime Timestamp where
  to = toLocalTime
  maybeFrom localTime =
    let timestamp = fromLocalTime localTime
     in if to timestamp == localTime
          then Just timestamp
          else Nothing

-- | Direct conversion from 'Data.Time.LocalTime'.
-- This is a total conversion as it always succeeds.
instance IsMany Time.LocalTime Timestamp where
  onfrom = fromLocalTime

-- | Format a LocalTime for PostgreSQL timestamp text format.
-- PostgreSQL requires specific formatting for extreme dates:
-- - Years must be 4-digit zero-padded for AD dates
-- - BC dates use "YYYY-MM-DD HH:MM:SS BC" format  
-- - Microseconds must be properly formatted
formatTimestampForPostgreSQL :: Time.LocalTime -> String
formatTimestampForPostgreSQL localTime =
  let day = Time.localDay localTime
      timeOfDay = Time.localTimeOfDay localTime
      (year, month, dayOfMonth) = Time.toGregorian day
      timeStr = formatTimeOfDay timeOfDay
   in if year <= 0
        then
          -- For BC dates (year <= 0), PostgreSQL expects format like "0001-01-01 00:00:00 BC"
          -- Note: year 0 is 1 BC, year -1 is 2 BC, etc.
          let bcYear = 1 - year
           in printf "%04d-%02d-%02d %s BC" (bcYear :: Integer) (month :: Int) (dayOfMonth :: Int) timeStr
        else
          -- For AD dates (year > 0), use zero-padded 4-digit year
          printf "%04d-%02d-%02d %s" (year :: Integer) (month :: Int) (dayOfMonth :: Int) timeStr
  where
    formatTimeOfDay :: Time.TimeOfDay -> String
    formatTimeOfDay tod =
      let h = Time.todHour tod :: Int
          m = Time.todMin tod :: Int
          pico = Time.todSec tod
          -- Convert from Pico to microseconds
          totalMicros = round (toRational pico * 1000000) :: Integer
          secs = totalMicros `div` 1000000 :: Integer
          micros = totalMicros `mod` 1000000 :: Integer
       in if micros == 0
            then printf "%02d:%02d:%02d" h m secs
            else printf "%02d:%02d:%02d.%06d" h m secs micros
