-- | PostgreSQL @timetz@ type.
-- Represents time with time zone.
module PrimitiveLayer.Primitives.Timetz (Timetz) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PrimitiveLayer.Primitives.Timetz.Offset as Offset
import qualified PrimitiveLayer.Primitives.Timetz.Time as Time
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import Text.Printf (printf)
import qualified TextBuilder

-- | PostgreSQL @timetz@ type representing time with time zone.
-- Stored as microseconds since midnight and timezone offset in seconds.
--
-- Low value: @00:00:00+1559@
--
-- High value: @24:00:00-1559@
--
-- https://www.postgresql.org/docs/17/datatype-datetime.html
data Timetz = Timetz
  { -- | Time as microseconds since midnight (00:00:00)
    time :: Time.TimetzTime,
    -- | Timezone offset in seconds (positive is east of UTC, negative is west of UTC)
    offset :: Offset.TimetzOffset
  }
  deriving stock (Eq, Ord, Generic)
  -- deriving stock (Show)
  deriving (Show) via (ViaPrimitive Timetz)

instance Arbitrary Timetz where
  arbitrary = do
    time <- arbitrary
    offset <- arbitrary
    pure (Timetz time offset)

instance Primitive Timetz where
  typeName = Tagged "timetz"

  baseOid = Tagged 1266

  arrayOid = Tagged 1270

  binaryEncoder (Timetz time offset) =
    mconcat
      [ Time.binaryEncoder time,
        Offset.binaryEncoder offset
      ]

  binaryDecoder =
    PeekyBlinders.statically do
      time <- Time.binaryDecoder
      offset <- Offset.binaryDecoder
      pure (Timetz <$> time <*> offset)

  -- Format:
  -- 23:59:59+15:59:59
  -- 24:00:00+15:59:59
  -- 00:00:00-15:59:59
  textualEncoder (Timetz time offset) =
    Time.renderInTextFormat time <> Offset.renderInTextFormat offset

-- | Convert from a tuple of TimeOfDay and timezone offset to Timetz.
instance IsSome (Time.TimeOfDay, Int32) Timetz where
  to (Timetz time offset) =
    let diffTime = fromIntegral (Time.toMicroseconds time) / 1_000_000
        timeOfDay = Time.timeToTimeOfDay diffTime
     in (timeOfDay, Offset.toSeconds offset)
  maybeFrom (timeOfDay, offset) =
    let diffTime = Time.timeOfDayToTime timeOfDay
        microseconds = round (diffTime * 1_000_000)
     in if microseconds >= 0 && microseconds < 86_400_000_000 -- 24 hours in microseconds
          then Just (Timetz (Time.TimetzTime microseconds) (Offset.TimetzOffset offset))
          else Nothing

-- | Convert from TimeOfDay and timezone offset, ensuring valid time range.
instance IsMany (Time.TimeOfDay, Int32) Timetz where
  from (timeOfDay, offset) =
    let diffTime = Time.timeOfDayToTime timeOfDay
        microseconds = round (diffTime * 1_000_000)
        -- Wrap around 24-hour period for negative values
        wrappedMicroseconds = microseconds `mod` 86_400_000_000
     in Timetz (Time.TimetzTime wrappedMicroseconds) (Offset.TimetzOffset offset)
