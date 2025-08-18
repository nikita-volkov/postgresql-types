-- | PostgreSQL @numeric@ type.
-- Represents arbitrary precision decimal numbers in PostgreSQL.
module PrimitiveLayer.Primitives.Numeric (Numeric) where

import qualified Data.ByteString as ByteString
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @numeric@ type wrapper around 'Data.Scientific.Scientific'.
newtype Numeric = Numeric Scientific.Scientific
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Numeric)

instance Primitive Numeric where
  typeName = Tagged "numeric"

  baseOid = Tagged 1700

  arrayOid = Tagged 1231

  binaryEncoder (Numeric x) =
    mconcat
      [ Write.bWord16 (fromIntegral componentsAmount),
        Write.bWord16 (fromIntegral pointIndex),
        signCode,
        Write.bWord16 (fromIntegral trimmedExponent),
        foldMap Write.bWord16 components
      ]
    where
      componentsAmount =
        length components
      coefficient =
        Scientific.coefficient x
      exponent =
        Scientific.base10Exponent x
      components =
        extractComponents tunedCoefficient
      pointIndex =
        componentsAmount + (tunedExponent `div` 4) - 1
      (tunedCoefficient, tunedExponent) =
        case mod exponent 4 of
          0 -> (coefficient, exponent)
          x -> (coefficient * 10 ^ x, exponent - x)
      trimmedExponent =
        if tunedExponent >= 0
          then 0
          else negate tunedExponent
      signCode =
        if coefficient < 0
          then Write.bWord16 0x4000
          else Write.bWord16 0x0000

  binaryDecoder = do
    (componentsAmount, pointIndex, signCode, _trimmedExponent) <- PeekyBlinders.statically do
      componentsAmount <- fromIntegral <$> PeekyBlinders.beSignedInt2
      pointIndex <- PeekyBlinders.beSignedInt2
      signCode <- PeekyBlinders.beUnsignedInt2
      trimmedExponent <- PeekyBlinders.beSignedInt2
      pure (componentsAmount, pointIndex, signCode, trimmedExponent)

    coefficient <- PeekyBlinders.statically do
      foldl' (\l r -> l * 10000 + fromIntegral r) 0
        <$> replicateM componentsAmount PeekyBlinders.beSignedInt2

    pure do
      signedCoefficient <- case signCode of
        0x0000 ->
          Right coefficient
        0x4000 ->
          Right (negate coefficient)
        0xC000 ->
          Left
            DecodingError
              { location = ["sign-code"],
                reason = UnexpectedValueDecodingErrorReason "Plus or Minus" "NaN"
              }
        _ ->
          Left
            DecodingError
              { location = ["sign-code"],
                reason =
                  UnexpectedValueDecodingErrorReason
                    "0x0000 or 0x4000"
                    (TextBuilder.toText (TextBuilder.decimal signCode))
              }
      let exponent = (fromIntegral pointIndex + 1 - componentsAmount) * 4
      pure (Numeric (Scientific.scientific signedCoefficient exponent))

  textualEncoder (Numeric scientific) =
    TextBuilder.text (fromString (Scientific.formatScientific Scientific.Fixed Nothing scientific))

-- | Direct conversion from 'Data.Scientific.Scientific'.
-- This is always safe since both types represent arbitrary precision decimals identically.
instance IsSome Scientific.Scientific Numeric where
  to (Numeric s) = s
  maybeFrom = Just . Numeric

-- | Direct conversion from 'Data.Scientific.Scientific'.
-- This is a total conversion as it always succeeds.
instance IsMany Scientific.Scientific Numeric where
  from = Numeric

{-# INLINE extractComponents #-}
extractComponents :: (Integral a) => a -> [Word16]
extractComponents =
  (reverse .) . (. abs) . unfoldr $ \case
    0 -> Nothing
    x -> case divMod x 10000 of
      (d, m) -> Just (fromIntegral m, d)
