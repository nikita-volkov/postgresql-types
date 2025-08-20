module PrimitiveLayer.Primitives.TimetzAsTimeOfDayAndTimeZone (TimetzAsTimeOfDayAndTimeZone) where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Primitives.Timetz (Timetz)
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder
import qualified TimeExtras.TimeOfDay as TimeOfDay
import qualified TimeExtras.TimeZone as TimeZone

-- | @timetz@. Time of day with time zone, represented as separate time and timezone components. Low value: @00:00:00+1559@. High value: @24:00:00-1559@.
--
-- <https://www.postgresql.org/docs/17/datatype-datetime.html#DATATYPE-TIMEZONES>
--
-- PostgreSQL @timetz@ type as separate time and timezone components.
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
    let time = TimeOfDay.toMicroseconds timeOfDay
        offset = TimeZone.toSeconds timeZone
     in from @(Int64, Int32) (fromIntegral time, fromIntegral offset)
  maybeFrom = Just . from

instance IsMany Timetz TimetzAsTimeOfDayAndTimeZone where
  from timetz =
    let (time, offset) = to @(Int64, Int32) timetz
     in TimetzAsTimeOfDayAndTimeZone
          (TimeOfDay.fromMicroseconds (fromIntegral time))
          (TimeZone.fromSeconds (fromIntegral offset))

instance IsSome (Time.TimeOfDay, Time.TimeZone) TimetzAsTimeOfDayAndTimeZone where
  to (TimetzAsTimeOfDayAndTimeZone timeOfDay timeZone) =
    (timeOfDay, timeZone)
  maybeFrom = Just . from

instance IsMany (Time.TimeOfDay, Time.TimeZone) TimetzAsTimeOfDayAndTimeZone where
  from (timeOfDay, timeZone) =
    let time = fromIntegral (TimeOfDay.toMicroseconds timeOfDay) :: Int64
        offset = fromIntegral (TimeZone.toSeconds timeZone) :: Int32
        timetz = from (time, offset) :: Timetz
     in from timetz
