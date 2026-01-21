module PostgresqlTypes.Types.Timetz.Time where

import qualified Data.Time as Time
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder
import qualified TimeExtras.TimeOfDay as TimeOfDay

-- | Time component of the @timetz@ type.
newtype TimetzTime = TimetzTime Int64
  deriving newtype (Eq, Ord, Show)

instance Arbitrary TimetzTime where
  arbitrary = TimetzTime <$> QuickCheck.choose (toMicroseconds minBound, toMicroseconds maxBound)

instance Bounded TimetzTime where
  minBound = TimetzTime 0
  maxBound = TimetzTime 86_400_000_000

toMicroseconds :: TimetzTime -> Int64
toMicroseconds (TimetzTime microseconds) = microseconds

toTimeOfDay :: TimetzTime -> Time.TimeOfDay
toTimeOfDay =
  TimeOfDay.convertFromMicroseconds . fromIntegral . toMicroseconds

projectFromMicroseconds :: Int64 -> Maybe TimetzTime
projectFromMicroseconds microseconds
  | microseconds >= toMicroseconds minBound && microseconds <= toMicroseconds maxBound = Just (TimetzTime microseconds)
  | otherwise = Nothing

projectFromPicoseconds :: Integer -> Maybe TimetzTime
projectFromPicoseconds picoseconds =
  let (microseconds, picoseconds') = divMod picoseconds 1_000_000
   in if picoseconds' == 0
        then projectFromMicroseconds (fromIntegral microseconds)
        else Nothing

projectFromTimeOfDay :: Time.TimeOfDay -> Maybe TimetzTime
projectFromTimeOfDay (Time.TimeOfDay hours minutes picoseconds) =
  let MkFixed picoseconds' = picoseconds
      picoseconds'' = fromIntegral ((hours * 60 + minutes) * 60) * 1_000_000_000_000 + picoseconds'
   in projectFromPicoseconds picoseconds''

-- | Wrap the overflow values around the clock.
normalizeFromMicroseconds :: Int64 -> TimetzTime
normalizeFromMicroseconds microseconds =
  let wrappedMicroseconds = microseconds `mod` 86_400_000_000
   in TimetzTime wrappedMicroseconds

normalizeFromPicoseconds :: Integer -> TimetzTime
normalizeFromPicoseconds picoseconds =
  let microseconds = fromIntegral (mod (div picoseconds 1_000_000) 86_400_000_000)
   in TimetzTime microseconds

normalizeFromTimeOfDay :: Time.TimeOfDay -> TimetzTime
normalizeFromTimeOfDay (Time.TimeOfDay hours minutes picoseconds) =
  let MkFixed picoseconds' = picoseconds
      picoseconds'' = fromIntegral ((hours * 60 + minutes) * 60) * 1_000_000_000_000 + picoseconds'
   in normalizeFromPicoseconds picoseconds''

renderInTextFormat :: TimetzTime -> TextBuilder.TextBuilder
renderInTextFormat (TimetzTime microseconds) =
  let (seconds, microseconds') = divMod microseconds 1_000_000
      (minutes, seconds') = divMod seconds 60
      (hours, minutes') = divMod minutes 60
   in mconcat
        [ TextBuilder.fixedLengthDecimal 2 hours,
          ":",
          TextBuilder.fixedLengthDecimal 2 minutes',
          ":",
          TextBuilder.fixedLengthDecimal 2 seconds',
          if microseconds == 0
            then ""
            else "." <> TextBuilder.fixedLengthDecimal 6 microseconds'
        ]

binaryEncoder :: TimetzTime -> Write.Write
binaryEncoder (TimetzTime microseconds) =
  Write.bInt64 microseconds

binaryDecoder :: PtrPeeker.Fixed (Either DecodingError TimetzTime)
binaryDecoder =
  PtrPeeker.beSignedInt8 <&> \int ->
    if int < toMicroseconds minBound
      then
        Left
          DecodingError
            { location = [],
              reason =
                UnsupportedValueDecodingErrorReason
                  "Value is less than minimum bound"
                  (TextBuilder.toText (TextBuilder.decimal int))
            }
      else
        if int > toMicroseconds maxBound
          then
            Left
              DecodingError
                { location = [],
                  reason =
                    UnsupportedValueDecodingErrorReason
                      "Value is greater than maximum bound"
                      (TextBuilder.toText (TextBuilder.decimal int))
                }
          else Right (TimetzTime int)
