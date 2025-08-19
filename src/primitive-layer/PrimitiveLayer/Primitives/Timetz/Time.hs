module PrimitiveLayer.Primitives.Timetz.Time where

import qualified Data.Time as Time
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

newtype TimetzTime = TimetzTime Int64
  deriving newtype (Eq, Ord, Show)

instance Arbitrary TimetzTime where
  arbitrary = TimetzTime <$> QuickCheck.choose (toMicroseconds minBound, toMicroseconds maxBound)

instance Bounded TimetzTime where
  minBound = TimetzTime 0
  maxBound = TimetzTime 86_400_000_000

toMicroseconds :: TimetzTime -> Int64
toMicroseconds (TimetzTime microseconds) = microseconds

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

binaryDecoder :: PeekyBlinders.Static (Either DecodingError TimetzTime)
binaryDecoder =
  PeekyBlinders.beSignedInt8 <&> \int ->
    if int < toMicroseconds minBound
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
        if int > toMicroseconds maxBound
          then
            Left
              DecodingError
                { location = [],
                  reason =
                    InvalidValueDecodingErrorReason
                      "Value is greater than maximum bound"
                      (TextBuilder.toText (TextBuilder.decimal int))
                }
          else Right (TimetzTime int)
