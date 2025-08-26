module PrimitiveLayer.Types.Timetz (Timetz) where

import qualified Data.Time
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PrimitiveLayer.Types.Timetz.Offset as Offset
import qualified PrimitiveLayer.Types.Timetz.Time as Time
import PrimitiveLayer.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import Text.Printf (printf)
import qualified TextBuilder

-- | PostgreSQL @timetz@ type. Time of day with time zone.
--
-- Stored as microseconds since midnight and timezone offset in seconds.
--
-- Low value: @00:00:00+1559@. High value: @24:00:00-1559@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-TIMEZONES).
data Timetz = Timetz
  { -- | Time as microseconds since midnight (00:00:00)
    time :: Time.TimetzTime,
    -- | Timezone offset in seconds (positive is east of UTC, negative is west of UTC)
    offset :: Offset.TimetzOffset
  }
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Timetz)

instance Arbitrary Timetz where
  arbitrary = do
    time <- arbitrary
    offset <- arbitrary
    pure (Timetz time offset)

instance Mapping Timetz where
  typeName = Tagged "timetz"

  baseOid = Tagged 1266

  arrayOid = Tagged 1270

  binaryEncoder (Timetz time offset) =
    mconcat
      [ Time.binaryEncoder time,
        Offset.binaryEncoder offset
      ]

  binaryDecoder =
    PtrPeeker.fixed do
      time <- Time.binaryDecoder
      offset <- Offset.binaryDecoder
      pure (Timetz <$> time <*> offset)

  -- Format:
  -- 23:59:59-15:59:59
  -- 24:00:00-15:59:59
  -- 00:00:00+15:59:59
  textualEncoder (Timetz time offset) =
    Time.renderInTextFormat time <> Offset.renderInTextFormat offset

-- | Convert from a tuple of time in microseconds and timezone offset in seconds to Timetz.
instance IsSome (Int64, Int32) Timetz where
  to (Timetz time offset) =
    (Time.toMicroseconds time, Offset.toSeconds offset)
  maybeFrom (microseconds, offset) = do
    time <- Time.compileFromMicroseconds microseconds
    offset <- Offset.compileFromSeconds offset
    pure (Timetz time offset)

-- | Normalize from time in microseconds and timezone offset in seconds, ensuring valid time range.
instance IsMany (Int64, Int32) Timetz where
  onfrom (time, offset) =
    Timetz (Time.normalizeFromMicroseconds time) (Offset.normalizeFromSeconds offset)

instance IsSome (Time.TimetzTime, Offset.TimetzOffset) Timetz where
  to (Timetz time offset) = (time, offset)

instance IsSome Timetz (Time.TimetzTime, Offset.TimetzOffset) where
  to (time, offset) = Timetz time offset

instance IsMany (Time.TimetzTime, Offset.TimetzOffset) Timetz

instance IsMany Timetz (Time.TimetzTime, Offset.TimetzOffset)

instance Is (Time.TimetzTime, Offset.TimetzOffset) Timetz

instance Is Timetz (Time.TimetzTime, Offset.TimetzOffset)
