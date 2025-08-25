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
  arbitrary = Timestamptz <$> QuickCheck.choose (0, maxBound)

instance Mapping Timestamptz where
  typeName = Tagged "timestamptz"
  baseOid = Tagged 1184
  arrayOid = Tagged 1185
  binaryEncoder (Timestamptz micros) = Write.bInt64 micros
  binaryDecoder = do
    microseconds <- PeekyBlinders.statically PeekyBlinders.beSignedInt8
    pure (Right (Timestamptz microseconds))
  textualEncoder (toUtcTime -> utcTime) =
    TextBuilder.string (Time.formatTime Time.defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%z" utcTime)

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
    let timestamptz = fromUtcTime utcTime
     in if to timestamptz == utcTime
          then Just timestamptz
          else Nothing

-- | Direct conversion from 'Data.Time.UTCTime'.
-- This is a total conversion as it always succeeds.
instance IsMany Time.UTCTime Timestamptz where
  onfrom = fromUtcTime
