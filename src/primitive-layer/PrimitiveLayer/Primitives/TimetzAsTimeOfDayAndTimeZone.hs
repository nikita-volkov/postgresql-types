module PrimitiveLayer.Primitives.TimetzAsTimeOfDayAndTimeZone (TimetzAsTimeOfDayAndTimeZone) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Primitives.Timetz (Timetz)
import qualified PrimitiveLayer.Primitives.Timetz.Offset as TimetzOffset
import qualified PrimitiveLayer.Primitives.Timetz.Time as TimetzTime
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder
import qualified TimeExtras.TimeOfDay as TimeOfDay
import qualified TimeExtras.TimeZone as TimeZone

-- | PostgreSQL @timetz@ type.
-- Time of day with time zone, represented as separate time and timezone components normalized to precisions of the data-types from the \"time\" library.
--
-- Low value: @00:00:00+1559@. High value: @24:00:00-1559@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-TIMEZONES)
data TimetzAsTimeOfDayAndTimeZone
  = TimetzAsTimeOfDayAndTimeZone
      Time.TimeOfDay
      Time.TimeZone
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive TimetzAsTimeOfDayAndTimeZone)
  deriving (Primitive) via (ViaIsMany Timetz TimetzAsTimeOfDayAndTimeZone)

instance Arbitrary TimetzAsTimeOfDayAndTimeZone where
  arbitrary = from @Timetz <$> arbitrary
  shrink (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone) =
    [ TimetzAsTimeOfDayAndTimeZone timeOfDay' timeZone'
    | (timeOfDay', timeZone') <- shrink (timeOfDay, timeZone)
    ]

instance IsSome Timetz TimetzAsTimeOfDayAndTimeZone where
  to (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone) =
    let time = TimeOfDay.compressToMicroseconds timeOfDay
        offset = TimeZone.convertToSeconds timeZone
     in from @(Int64, Int32) (fromIntegral time, fromIntegral offset)
  maybeFrom timetz = do
    let (timeMicroseconds, offsetSeconds) = to @(Int64, Int32) timetz
    let timeOfDay = TimeOfDay.convertFromMicroseconds (fromIntegral timeMicroseconds)
    -- Check if conversion is lossless by converting back and comparing
    let timeZone = TimeZone.compressFromSeconds (fromIntegral offsetSeconds)
    let reconstructedOffsetSeconds = fromIntegral (TimeZone.convertToSeconds timeZone)
    if reconstructedOffsetSeconds == offsetSeconds
      then pure (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone)
      else Nothing

instance IsMany Timetz TimetzAsTimeOfDayAndTimeZone where
  from timetz =
    let (time, offset) = to @(Int64, Int32) timetz
        timeOfDay = TimeOfDay.convertFromMicroseconds (fromIntegral time)
        -- Use the same validation logic as IsSome, but fallback to normalized offset if validation fails
        timeZone = case TimeZone.compileFromSeconds (fromIntegral offset) of
          Just tz -> tz
          Nothing -> TimeZone.compressFromSeconds (fromIntegral offset)
     in TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone

instance IsSome (Time.TimeOfDay, Time.TimeZone) TimetzAsTimeOfDayAndTimeZone where
  to (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone) =
    (timeOfDay, timeZone)
  maybeFrom (timeOfDay, timeZone) = do
    time <- TimeOfDay.compileToMicroseconds timeOfDay
    let offset = fromIntegral (TimeZone.convertToSeconds timeZone)
    timetz :: Timetz <- maybeFrom (fromIntegral time :: Int64, offset :: Int32)
    maybeFrom timetz

instance IsMany (Time.TimeOfDay, Time.TimeZone) TimetzAsTimeOfDayAndTimeZone where
  from (timeOfDay, timeZone) =
    let time = fromIntegral (TimeOfDay.compressToMicroseconds timeOfDay) :: Int64
        offset = fromIntegral (TimeZone.convertToSeconds timeZone) :: Int32
        timetz = from (time, offset) :: Timetz
     in from timetz
