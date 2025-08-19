module PrimitiveLayer.Primitives.TimetzInTimeOfDayAndTimeZone (TimetzInTimeOfDayAndTimeZone) where

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

data TimetzInTimeOfDayAndTimeZone
  = TimetzInTimeOfDayAndTimeZone
      Time.TimeOfDay
      Time.TimeZone
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive TimetzInTimeOfDayAndTimeZone)
  deriving (Primitive) via (ViaIsMany Timetz TimetzInTimeOfDayAndTimeZone)

instance Arbitrary TimetzInTimeOfDayAndTimeZone where
  arbitrary = from @Timetz <$> arbitrary
  shrink (TimetzInTimeOfDayAndTimeZone timeOfDay timeZone) =
    [ TimetzInTimeOfDayAndTimeZone timeOfDay' timeZone'
    | (timeOfDay', timeZone') <- shrink (timeOfDay, timeZone)
    ]

instance IsSome Timetz TimetzInTimeOfDayAndTimeZone where
  to (TimetzInTimeOfDayAndTimeZone timeOfDay timeZone) =
    let time = TimeOfDay.toMicroseconds timeOfDay
        offset = TimeZone.toSeconds timeZone
     in from @(Int64, Int32) (fromIntegral time, fromIntegral offset)
  maybeFrom = Just . from

instance IsMany Timetz TimetzInTimeOfDayAndTimeZone where
  from timetz =
    let (time, offset) = to @(Int64, Int32) timetz
     in TimetzInTimeOfDayAndTimeZone
          (TimeOfDay.fromMicroseconds (fromIntegral time))
          (TimeZone.fromSeconds (fromIntegral offset))

instance IsSome (Time.TimeOfDay, Time.TimeZone) TimetzInTimeOfDayAndTimeZone where
  to (TimetzInTimeOfDayAndTimeZone timeOfDay timeZone) =
    (timeOfDay, timeZone)
  maybeFrom = Just . from

instance IsMany (Time.TimeOfDay, Time.TimeZone) TimetzInTimeOfDayAndTimeZone where
  from (timeOfDay, timeZone) =
    let time = fromIntegral (TimeOfDay.toMicroseconds timeOfDay) :: Int64
        offset = fromIntegral (TimeZone.toSeconds timeZone) :: Int32
        timetz = from (time, offset) :: Timetz
     in from timetz
