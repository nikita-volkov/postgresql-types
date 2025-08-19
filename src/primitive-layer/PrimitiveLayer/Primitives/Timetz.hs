-- | PostgreSQL @timetz@ type.
-- Represents time with time zone.
module PrimitiveLayer.Primitives.Timetz (Timetz (..)) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import Text.Printf (printf)
import qualified TextBuilder

-- | PostgreSQL @timetz@ type representing time with time zone.
-- Stored as microseconds since midnight and timezone offset in seconds.
data Timetz = Timetz
  { -- | Time as microseconds since midnight (00:00:00)
    timetzTime :: Int64,
    -- | Timezone offset in seconds (negative is west of UTC)
    timetzOffset :: Int32
  }
  deriving stock (Eq, Ord, Generic)
  deriving (Show) via (ViaPrimitive Timetz)

instance Arbitrary Timetz where
  arbitrary = do
    -- Generate hours, minutes, seconds directly to avoid floating point precision issues
    hours <- QuickCheck.choose (0, 23)
    minutes <- QuickCheck.choose (0, 59)
    seconds <- QuickCheck.choose (0, 59)
    microseconds <- QuickCheck.choose (0, 999999)
    timezone <- QuickCheck.choose (-12 * 3600, 14 * 3600) -- UTC-12 to UTC+14
    let totalMicroseconds = fromIntegral hours * 3600 * 1_000_000 +
                           fromIntegral minutes * 60 * 1_000_000 +
                           fromIntegral seconds * 1_000_000 +
                           fromIntegral microseconds
    pure (Timetz totalMicroseconds timezone)
  shrink (Timetz time offset) =
    [Timetz time' offset' | (time', offset') <- shrink (time, offset), 
     time' >= 0 && time' < 86_400_000_000] -- Ensure time is within 24-hour period

instance Primitive Timetz where
  typeName = Tagged "timetz"
  baseOid = Tagged 1266
  arrayOid = Tagged 1270
  binaryEncoder (Timetz time offset) =
    mconcat
      [ Write.bInt64 time,
        Write.bInt32 offset
      ]
  binaryDecoder = do
    time <- PeekyBlinders.statically PeekyBlinders.beSignedInt8
    offset <- PeekyBlinders.statically PeekyBlinders.beSignedInt4
    pure (Right (Timetz time offset))
  textualEncoder (Timetz time offset) =
    let diffTime = fromIntegral time / 1_000_000
        timeOfDay = Time.timeToTimeOfDay diffTime
        offsetHours = offset `div` 3600
        offsetMinutes = abs (offset `mod` 3600) `div` 60
        offsetSign = if offset >= 0 then "+" else "-"
        -- Format time without fractional seconds if they are zero to match PostgreSQL output
        timeStr = if time `mod` 1_000_000 == 0
                    then Time.formatTime Time.defaultTimeLocale "%H:%M:%S" timeOfDay
                    else Time.formatTime Time.defaultTimeLocale "%H:%M:%S%Q" timeOfDay
        offsetStr = offsetSign <> printf "%02d" (abs offsetHours) <> ":" <> printf "%02d" offsetMinutes
     in TextBuilder.string (timeStr <> offsetStr)

-- | Convert from a tuple of TimeOfDay and timezone offset to Timetz.
instance IsSome (Time.TimeOfDay, Int32) Timetz where
  to (Timetz time offset) =
    let diffTime = fromIntegral time / 1_000_000
        timeOfDay = Time.timeToTimeOfDay diffTime
     in (timeOfDay, offset)
  maybeFrom (timeOfDay, offset) =
    let diffTime = Time.timeOfDayToTime timeOfDay
        microseconds = round (diffTime * 1_000_000)
     in if microseconds >= 0 && microseconds < 86_400_000_000 -- 24 hours in microseconds
          then Just (Timetz microseconds offset)
          else Nothing

-- | Convert from Timetz to a tuple of TimeOfDay and timezone offset.
instance IsSome Timetz (Time.TimeOfDay, Int32) where
  to (timeOfDay, offset) =
    let diffTime = Time.timeOfDayToTime timeOfDay
        microseconds = round (diffTime * 1_000_000)
     in Timetz microseconds offset
  maybeFrom (Timetz time offset) =
    let diffTime = fromIntegral time / 1_000_000
        timeOfDay = Time.timeToTimeOfDay diffTime
     in Just (timeOfDay, offset)

-- | Convert from TimeOfDay and timezone offset, ensuring valid time range.
instance IsMany (Time.TimeOfDay, Int32) Timetz where
  from (timeOfDay, offset) =
    let diffTime = Time.timeOfDayToTime timeOfDay
        microseconds = round (diffTime * 1_000_000)
        -- Wrap around 24-hour period for negative values
        wrappedMicroseconds = microseconds `mod` 86_400_000_000
     in Timetz wrappedMicroseconds offset

-- | Convert from Timetz to a tuple of TimeOfDay and timezone offset.
instance IsMany Timetz (Time.TimeOfDay, Int32) where
  from (Timetz time offset) =
    let diffTime = fromIntegral time / 1_000_000
        timeOfDay = Time.timeToTimeOfDay diffTime
     in (timeOfDay, offset)

-- | Bidirectional conversion between tuple and Timetz.
instance Is (Time.TimeOfDay, Int32) Timetz

instance Is Timetz (Time.TimeOfDay, Int32)
