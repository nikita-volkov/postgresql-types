module PrimitiveLayer.Primitives.Timetz.Offset where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

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

compileFromSeconds :: Int32 -> Maybe TimetzOffset
compileFromSeconds seconds
  | seconds >= toSeconds minBound && seconds <= toSeconds maxBound = Just (TimetzOffset seconds)
  | otherwise = Nothing

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

binaryDecoder :: PeekyBlinders.Static (Either DecodingError TimetzOffset)
binaryDecoder =
  PeekyBlinders.beSignedInt4 <&> \int ->
    if int < toSeconds minBound
      then
        Left
          DecodingError
            { location = [],
              reason =
                InvalidValueDecodingErrorReason
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
                    InvalidValueDecodingErrorReason
                      "Value is greater than maximum bound"
                      (TextBuilder.toText (TextBuilder.decimal int))
                }
          else Right (TimetzOffset int)
