module PrimitiveLayer.Types.Timestamp (Timestamp) where

import qualified Data.Time as Time
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPeeker
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
  deriving (Show) via (ViaIsPrimitive Timestamp)

instance Arbitrary Timestamp where
  arbitrary = Timestamp <$> QuickCheck.choose (pgTimestampMin, pgTimestampMax)
    where
      -- PostgreSQL's actual documented timestamp range: 4713 BC to 294276 AD
      -- Do not artificially restrict to avoid edge cases - let the tests expose real issues
      pgTimestampMin = -210866803200000000 -- 4713 BC January 1 00:00:00
      pgTimestampMax = 9214646400000000000 -- 294276 AD December 31 23:59:59.999999

instance IsPrimitive Timestamp where
  typeName = Tagged "timestamp"
  baseOid = Tagged 1114
  arrayOid = Tagged 1115
  binaryEncoder (Timestamp micros) = Write.bInt64 micros
  binaryDecoder = do
    microseconds <- PtrPeeker.fixed PtrPeeker.beSignedInt8
    pure (Right (Timestamp microseconds))
  textualEncoder (toLocalTime -> localTime) =
    formatTimestampForPostgreSQL localTime

-- | Mapping to @tsrange@ type.
instance IsRangeElement Timestamp where
  rangeTypeName = Tagged "tsrange"
  rangeOid = Tagged 3908
  rangeArrayOid = Tagged 3909

-- | Mapping to @tsmultirange@ type.
instance IsMultirangeElement Timestamp where
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
formatTimestampForPostgreSQL :: Time.LocalTime -> TextBuilder
formatTimestampForPostgreSQL localTime =
  let day = Time.localDay localTime
      timeOfDay = Time.localTimeOfDay localTime
      (year, month, dayOfMonth) = Time.toGregorian day
      timeBuilder = formatTimeOfDay timeOfDay
   in if year <= 0
        then
          -- For BC dates (year <= 0), PostgreSQL expects format like "0001-01-01 00:00:00 BC"
          -- Note: year 0 is 1 BC, year -1 is 2 BC, etc.
          let bcYear = negate (1 - year)
           in mconcat
                [ if bcYear <= 999
                    then TextBuilder.fixedLengthDecimal 4 (bcYear :: Integer)
                    else TextBuilder.decimal (bcYear :: Integer),
                  "-",
                  TextBuilder.fixedLengthDecimal 2 (fromIntegral month :: Integer),
                  "-",
                  TextBuilder.fixedLengthDecimal 2 (fromIntegral dayOfMonth :: Integer),
                  " ",
                  timeBuilder,
                  " BC"
                ]
        else
          -- For AD dates (year > 0), use zero-padded 4-digit year
          mconcat
            [ if year <= 999
                then TextBuilder.fixedLengthDecimal 4 (year :: Integer)
                else TextBuilder.decimal (year :: Integer),
              "-",
              TextBuilder.fixedLengthDecimal 2 (fromIntegral month :: Integer),
              "-",
              TextBuilder.fixedLengthDecimal 2 (fromIntegral dayOfMonth :: Integer),
              " ",
              timeBuilder
            ]
  where
    formatTimeOfDay :: Time.TimeOfDay -> TextBuilder
    formatTimeOfDay tod =
      let h = Time.todHour tod :: Int
          m = Time.todMin tod :: Int
          pico = Time.todSec tod
          -- Convert from Pico to microseconds
          totalMicros = round (toRational pico * 1000000) :: Integer
          secs = totalMicros `div` 1000000 :: Integer
          micros = totalMicros `mod` 1000000 :: Integer
       in if micros == 0
            then
              mconcat
                [ TextBuilder.fixedLengthDecimal 2 (fromIntegral h :: Integer),
                  ":",
                  TextBuilder.fixedLengthDecimal 2 (fromIntegral m :: Integer),
                  ":",
                  TextBuilder.fixedLengthDecimal 2 secs
                ]
            else
              mconcat
                [ TextBuilder.fixedLengthDecimal 2 (fromIntegral h :: Integer),
                  ":",
                  TextBuilder.fixedLengthDecimal 2 (fromIntegral m :: Integer),
                  ":",
                  TextBuilder.fixedLengthDecimal 2 secs,
                  ".",
                  TextBuilder.fixedLengthDecimal 6 micros
                ]
