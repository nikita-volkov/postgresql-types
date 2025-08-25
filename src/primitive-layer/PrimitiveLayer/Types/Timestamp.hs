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
  arbitrary = Timestamp <$> QuickCheck.choose (0, maxBound)

instance Mapping Timestamp where
  typeName = Tagged "timestamp"
  baseOid = Tagged 1114
  arrayOid = Tagged 1115
  binaryEncoder (Timestamp micros) = Write.bInt64 micros
  binaryDecoder = do
    microseconds <- PeekyBlinders.statically PeekyBlinders.beSignedInt8
    pure (Right (Timestamp microseconds))
  textualEncoder (toLocalTime -> localTime) =
    TextBuilder.string (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" localTime)

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
