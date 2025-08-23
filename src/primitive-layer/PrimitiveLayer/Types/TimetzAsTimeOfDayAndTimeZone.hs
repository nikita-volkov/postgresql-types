module PrimitiveLayer.Types.TimetzAsTimeOfDayAndTimeZone (TimetzAsTimeOfDayAndTimeZone) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Types.Timetz (Timetz)
import qualified PrimitiveLayer.Types.Timetz.Offset as TimetzOffset
import qualified PrimitiveLayer.Types.Timetz.Time as TimetzTime
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
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-TIMEZONES).
data TimetzAsTimeOfDayAndTimeZone
  = TimetzAsTimeOfDayAndTimeZone
      Time.TimeOfDay
      Time.TimeZone
  deriving stock (Eq, Ord)
  deriving (Mapping, Arbitrary, Show) via (ViaIsMany Timetz TimetzAsTimeOfDayAndTimeZone)

instance IsSome Timetz TimetzAsTimeOfDayAndTimeZone where
  to (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone) =
    let time = TimeOfDay.compressToMicroseconds timeOfDay
        offset = TimeZone.convertToSeconds timeZone
     in onfrom @(Int64, Int32) (fromIntegral time, fromIntegral offset)
  maybeFrom timetz = do
    let (timeMicroseconds, offsetSeconds) = to @(Int64, Int32) timetz
    let timeOfDay = TimeOfDay.convertFromMicroseconds (fromIntegral timeMicroseconds)
    timeZone <- TimeZone.compileFromSeconds (fromIntegral offsetSeconds)
    pure (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone)

instance IsMany Timetz TimetzAsTimeOfDayAndTimeZone where
  onfrom timetz =
    let (time, offset) = to @(Int64, Int32) timetz
     in TimetzAsTimeOfDayAndTimeZone
          (TimeOfDay.convertFromMicroseconds (fromIntegral time))
          (TimeZone.compressFromSeconds (fromIntegral offset))

instance IsSome (Time.TimeOfDay, Time.TimeZone) TimetzAsTimeOfDayAndTimeZone where
  to (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone) =
    (timeOfDay, timeZone)
  maybeFrom (timeOfDay, timeZone) = do
    time <- TimeOfDay.compileToMicroseconds timeOfDay
    let offset = fromIntegral (TimeZone.convertToSeconds timeZone)
    timetz :: Timetz <- maybeFrom (fromIntegral time :: Int64, offset :: Int32)
    maybeFrom timetz

instance IsMany (Time.TimeOfDay, Time.TimeZone) TimetzAsTimeOfDayAndTimeZone where
  onfrom (timeOfDay, timeZone) =
    let time = fromIntegral (TimeOfDay.compressToMicroseconds timeOfDay) :: Int64
        offset = fromIntegral (TimeZone.convertToSeconds timeZone) :: Int32
        timetz = onfrom (time, offset) :: Timetz
     in onfrom timetz
