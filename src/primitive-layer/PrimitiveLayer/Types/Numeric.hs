module PrimitiveLayer.Types.Numeric (Numeric (..)) where

import qualified Data.ByteString as ByteString
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @numeric@ type. Arbitrary precision decimal number.
--
-- Up to @131072@ digits before decimal point, up to @16383@ digits after decimal point.
--
-- On the Haskell end the 'Scientific.Scientific' type fits well with an exception of it not supporting @NaN@ values, which Postgres does support.
-- Hence is why we represent it as a sum-type with a separate constructor for @NaN@-values.
--
-- The 'IsMany' and 'IsSome' instances provide bidirectional conversions for convenience.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-numeric.html#DATATYPE-NUMERIC-DECIMAL).
data Numeric
  = ScientificNumeric Scientific.Scientific
  | NanNumeric
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaPrimitive Numeric)

instance Arbitrary Numeric where
  arbitrary =
    QuickCheck.oneof
      [ ScientificNumeric <$> arbitrary,
        pure NanNumeric
      ]

instance Mapping Numeric where
  typeName = Tagged "numeric"

  baseOid = Tagged 1700

  arrayOid = Tagged 1231

  binaryEncoder = \case
    ScientificNumeric x ->
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
    NanNumeric ->
      mconcat
        [ Write.bWord16 0x0000, -- componentsAmount
          Write.bWord16 0x0000, -- pointIndex
          Write.bWord16 0xC000, -- signCode for NaN
          Write.bWord16 0x0000 -- trimmedExponent
        ]

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

    pure
      let byCoefficient coefficient =
            let exponent = (fromIntegral pointIndex + 1 - componentsAmount) * 4
             in Right (ScientificNumeric (Scientific.scientific coefficient exponent))
       in case signCode of
            0x0000 -> byCoefficient coefficient
            0x4000 -> byCoefficient (negate coefficient)
            0xC000 -> Right NanNumeric
            _ ->
              Left
                DecodingError
                  { location = ["sign-code"],
                    reason =
                      UnexpectedValueDecodingErrorReason
                        "0x0000 or 0x4000"
                        (TextBuilder.toText (TextBuilder.decimal signCode))
                  }

  textualEncoder = \case
    ScientificNumeric scientific ->
      TextBuilder.text (fromString (Scientific.formatScientific Scientific.Fixed Nothing scientific))
    NanNumeric ->
      TextBuilder.text "NaN"

instance RangeMapping Numeric where
  rangeTypeName = Tagged "numrange"
  rangeOid = Tagged 3906
  rangeArrayOid = Tagged 3907

-- |
-- In 'maybeFrom' produces 'Nothing' for 'NanNumeric' values.
instance IsSome Numeric Scientific.Scientific where
  to = ScientificNumeric
  maybeFrom = \case
    ScientificNumeric s -> Just s
    NanNumeric -> Nothing

-- | Treats 'NanNumeric' values as @0@.
instance IsMany Numeric Scientific.Scientific where
  onfrom = \case
    ScientificNumeric s -> s
    NanNumeric -> 0

{-# INLINE extractComponents #-}
extractComponents :: (Integral a) => a -> [Word16]
extractComponents =
  (reverse .) . (. abs) . unfoldr $ \case
    0 -> Nothing
    x -> case divMod x 10000 of
      (d, m) -> Just (fromIntegral m, d)
