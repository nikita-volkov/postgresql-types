module PostgresqlTypes.Types.Timetz.Offset where

import qualified Data.Time as Time
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | Offset component of the @timetz@ type.
newtype TimetzOffset = TimetzOffset Int32
  deriving newtype (Eq, Ord, Show)

instance Arbitrary TimetzOffset where
  arbitrary = TimetzOffset <$> QuickCheck.choose (toSeconds minBound, toSeconds maxBound)

instance Bounded TimetzOffset where
  minBound = TimetzOffset (negate extemeInSeconds)
  maxBound = TimetzOffset extemeInSeconds

-- | @15:59:59@
extemeInSeconds :: Int32
extemeInSeconds =
  59 + 59 * 60 + 15 * 60 * 60

toSeconds :: TimetzOffset -> Int32
toSeconds (TimetzOffset seconds) = seconds

toTimeZone :: TimetzOffset -> Time.TimeZone
toTimeZone (TimetzOffset seconds) =
  let -- Round to the nearest minute instead of truncating
      minutes = fromIntegral (seconds + 30) `div` 60
   in Time.minutesToTimeZone minutes

projectFromSeconds :: Int32 -> Maybe TimetzOffset
projectFromSeconds seconds
  | seconds >= toSeconds minBound && seconds <= toSeconds maxBound = Just (TimetzOffset seconds)
  | otherwise = Nothing

projectFromTimeZone :: Time.TimeZone -> Maybe TimetzOffset
projectFromTimeZone (Time.TimeZone minutes _ _) =
  let seconds = fromIntegral (minutes * 60)
   in projectFromSeconds seconds

-- | Clamp seconds to the valid range.
normalizeFromSeconds :: Int32 -> TimetzOffset
normalizeFromSeconds seconds =
  if seconds < toSeconds minBound
    then minBound
    else
      if seconds > toSeconds maxBound
        then maxBound
        else TimetzOffset seconds

normalizeFromTimeZone :: Time.TimeZone -> TimetzOffset
normalizeFromTimeZone (Time.TimeZone minutes _ _) =
  let seconds = fromIntegral (minutes * 60)
   in normalizeFromSeconds seconds

renderInTextFormat :: TimetzOffset -> TextBuilder.TextBuilder
renderInTextFormat (TimetzOffset seconds) =
  let (sign, seconds') = if seconds < 0 then ("+", negate seconds) else ("-", seconds)
      (minutes, seconds'') = divMod seconds' 60
      (hours, minutes') = divMod minutes 60
   in mconcat
        [ sign,
          TextBuilder.fixedLengthDecimal 2 hours,
          ":",
          TextBuilder.fixedLengthDecimal 2 minutes',
          ":",
          TextBuilder.fixedLengthDecimal 2 seconds''
        ]

binaryEncoder :: TimetzOffset -> Write.Write
binaryEncoder (TimetzOffset seconds) =
  Write.bInt32 seconds

binaryDecoder :: PtrPeeker.Fixed (Either DecodingError TimetzOffset)
binaryDecoder =
  PtrPeeker.beSignedInt4 <&> \int ->
    if int < toSeconds minBound
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
        if int > toSeconds maxBound
          then
            Left
              DecodingError
                { location = [],
                  reason =
                    UnsupportedValueDecodingErrorReason
                      "Value is greater than maximum bound"
                      (TextBuilder.toText (TextBuilder.decimal int))
                }
          else Right (TimetzOffset int)
