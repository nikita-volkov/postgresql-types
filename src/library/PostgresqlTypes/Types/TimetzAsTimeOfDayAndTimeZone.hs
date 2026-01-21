module PostgresqlTypes.Types.TimetzAsTimeOfDayAndTimeZone (TimetzAsTimeOfDayAndTimeZone (..)) where

import qualified Data.Time as Time
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Types.Timetz (Timetz)
import PostgresqlTypes.Via
import qualified TimeExtras.TimeOfDay as TimeOfDay
import qualified TimeExtras.TimeZone as TimeZone

-- | PostgreSQL @timetz@ type.
-- Time of day with time zone, represented as separate time and timezone components normalized to precisions of the data-types from the \"time\" library.
--
-- Low value: @00:00:00+1559@. High value: @24:00:00-1559@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-datetime.html#DATATYPE-TIMEZONES).
data TimetzAsTimeOfDayAndTimeZone
  = TimetzAsTimeOfDayAndTimeZone
      Time.TimeOfDay
      Time.TimeZone
  deriving stock (Eq, Ord)
  deriving (IsScalar, Arbitrary, Show) via (ViaIsMany Timetz TimetzAsTimeOfDayAndTimeZone)

instance Bounded TimetzAsTimeOfDayAndTimeZone where
  minBound = TimetzAsTimeOfDayAndTimeZone (Time.TimeOfDay 0 0 0) (TimeZone.convertFromMinutes (negate (15 * 60 + 59)))
  maxBound = TimetzAsTimeOfDayAndTimeZone (Time.TimeOfDay 24 0 0) (TimeZone.convertFromMinutes (15 * 60 + 59))

instance IsSome Timetz TimetzAsTimeOfDayAndTimeZone where
  to (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone) =
    let time = TimeOfDay.normalizeToMicroseconds timeOfDay
        offset = TimeZone.convertToSeconds timeZone
     in onfrom @(Int64, Int32) (fromIntegral time, fromIntegral offset)
  maybeFrom timetz = do
    let (timeMicroseconds, offsetSeconds) = to @(Int64, Int32) timetz
    let timeOfDay = TimeOfDay.convertFromMicroseconds (fromIntegral timeMicroseconds)
    timeZone <- TimeZone.projectFromSeconds (fromIntegral offsetSeconds)
    pure (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone)

instance IsMany Timetz TimetzAsTimeOfDayAndTimeZone where
  onfrom timetz =
    let (time, offset) = to @(Int64, Int32) timetz
     in TimetzAsTimeOfDayAndTimeZone
          (TimeOfDay.convertFromMicroseconds (fromIntegral time))
          (TimeZone.normalizeFromSeconds (fromIntegral offset))

instance IsSome (Time.TimeOfDay, Time.TimeZone) TimetzAsTimeOfDayAndTimeZone where
  to (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone) =
    (timeOfDay, timeZone)
  maybeFrom (timeOfDay, timeZone) = do
    time <- TimeOfDay.projectToMicroseconds timeOfDay
    let offset = fromIntegral (TimeZone.convertToSeconds timeZone)
    timetz :: Timetz <- maybeFrom (fromIntegral time :: Int64, offset :: Int32)
    maybeFrom timetz

instance IsMany (Time.TimeOfDay, Time.TimeZone) TimetzAsTimeOfDayAndTimeZone where
  onfrom (timeOfDay, timeZone) =
    let time = fromIntegral (TimeOfDay.normalizeToMicroseconds timeOfDay) :: Int64
        offset = fromIntegral (TimeZone.convertToSeconds timeZone) :: Int32
        timetz = onfrom (time, offset) :: Timetz
     in onfrom timetz
