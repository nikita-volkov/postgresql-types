-- Reference implementation for numeric type in PostgreSQL:
--
-- - Sign flags: https://github.com/postgres/postgres/blob/6bca4b50d000e840cad17a9dd6cb46785fb2cedb/src/backend/utils/adt/numeric.c#L201-L203
module PostgresqlTypes.Types.Numeric
  ( Numeric,

    -- * Accessors.
    isNaN,
    isPosInfinity,
    isNegInfinity,
    normalizeToScientific,
    refineToScientific,

    -- * Constructors.
    nan,
    posInfinity,
    negInfinity,
    normalizeFromScientific,
    refineFromScientific,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified GHC.TypeLits as TypeLits
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (isNaN)
import qualified PostgresqlTypes.Types.Numeric.Integer as Integer
import qualified PostgresqlTypes.Types.Numeric.Scientific as Scientific
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @numeric@ or @decimal@ type. Arbitrary or specific precision decimal number.
--
-- Use @Numeric 0 0@ to represent @numeric@ without precision/scale constraints (arbitrary precision).
--
-- Up to @131072@ digits before decimal point, up to @16383@ digits after decimal point.
--
-- On the Haskell end the 'Scientific.Scientific' type fits almost well with a few corner cases of it not supporting @NaN@, @Infinity@ and @-Infinity@ values, which Postgres does support.
-- Please notice that @Infinity@ and @-Infinity@ values are not supported by Postgres versions lower than 14.
--
-- The type parameters @precision@ and @scale@ specify the static precision and scale of the numeric value.
-- Only numeric values conforming to these constraints can be represented by this type.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-numeric.html#DATATYPE-NUMERIC-DECIMAL).
data Numeric (precision :: TypeLits.Nat) (scale :: TypeLits.Nat)
  = NegInfinityNumeric
  | ScientificNumeric Scientific.Scientific
  | PosInfinityNumeric
  | NanNumeric
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsScalar (Numeric precision scale))

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => Arbitrary (Numeric precision scale) where
  arbitrary =
    let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
        sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
        intDigits = max 0 (prec - sc)
     in if prec == 0 && sc == 0
          then
            -- Arbitrary precision numeric: generate any Scientific value or special values
            QuickCheck.frequency
              [ (1, pure NanNumeric),
                (1, pure NegInfinityNumeric),
                (1, pure PosInfinityNumeric),
                (47, ScientificNumeric . Scientific.normalize <$> arbitrary)
              ]
          else
            if sc > prec
              then
                -- Invalid configuration: scale cannot be greater than precision
                pure NanNumeric
              else
                -- From PostgreSQL docs:
                -- Note that an infinity can only be stored in an unconstrained numeric column, because it notionally exceeds any finite precision limit.
                QuickCheck.frequency
                  [ (1, pure NanNumeric),
                    ( 49,
                      do
                        -- Generate value respecting precision and scale constraints
                        -- Precision p, scale s means: at most p total digits, s after decimal point
                        -- At this point sc <= prec (sc > prec is handled above), so intDigits is non-negative.

                        -- Generate a value with appropriate number of digits
                        negative <- arbitrary @Bool
                        -- Generate integer part (up to intDigits digits)
                        intPart <-
                          if intDigits > 0
                            then QuickCheck.choose (0, 10 ^ intDigits - 1)
                            else pure 0
                        -- Generate fractional part (up to sc digits)
                        fracPart <-
                          if sc > 0
                            then QuickCheck.choose (0, 10 ^ sc - 1)
                            else pure 0
                        let unsignedCoefficient = intPart * (10 ^ sc) + fracPart
                            coefficient = if negative then negate unsignedCoefficient else unsignedCoefficient
                            scientific = Scientific.scientific coefficient (negate (fromIntegral sc))
                        pure (ScientificNumeric scientific)
                    )
                  ]

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsScalar (Numeric precision scale) where
  typeName = Tagged "numeric"
  baseOid = Tagged (Just 1700)
  arrayOid = Tagged (Just 1231)
  typeParams =
    Tagged
      ( let prec = TypeLits.natVal (Proxy @precision)
            sc = TypeLits.natVal (Proxy @scale)
         in case (prec, sc) of
              (0, 0) -> [] -- No type modifiers for arbitrary precision numeric
              (p, 0) -> [Text.pack (show p)] -- numeric(precision)
              (p, s) -> [Text.pack (show p), Text.pack (show s)] -- numeric(precision, scale)
      )

  binaryEncoder = \case
    ScientificNumeric x ->
      mconcat
        [ Write.bWord16 (fromIntegral componentsAmount),
          Write.bWord16 (fromIntegral pointIndex),
          flag,
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
          Integer.extractComponents tunedCoefficient
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
        flag =
          if coefficient < 0
            then Write.bWord16 0x4000
            else Write.bWord16 0x0000
    NanNumeric ->
      mconcat
        [ Write.bWord16 0x0000, -- componentsAmount
          Write.bWord16 0x0000, -- pointIndex
          Write.bWord16 0xC000, -- flag for NaN
          Write.bWord16 0x0000 -- trimmedExponent
        ]
    PosInfinityNumeric ->
      mconcat
        [ Write.bWord16 0x0000, -- componentsAmount
          Write.bWord16 0x0000, -- pointIndex
          Write.bWord16 0xD000, -- flag for +Infinity
          Write.bWord16 0x0000 -- trimmedExponent
        ]
    NegInfinityNumeric ->
      mconcat
        [ Write.bWord16 0x0000, -- componentsAmount
          Write.bWord16 0x0000, -- pointIndex
          Write.bWord16 0xF000, -- flag for -Infinity
          Write.bWord16 0x0000 -- trimmedExponent
        ]

  binaryDecoder =
    let prec = fromIntegral (TypeLits.natVal (Proxy @precision))
        sc = fromIntegral (TypeLits.natVal (Proxy @scale))
     in do
          (componentsAmount, pointIndex, flag, _trimmedExponent) <- PtrPeeker.fixed do
            componentsAmount <- fromIntegral <$> PtrPeeker.beSignedInt2
            pointIndex <- PtrPeeker.beSignedInt2
            flag <- PtrPeeker.beUnsignedInt2
            trimmedExponent <- PtrPeeker.beSignedInt2
            pure (componentsAmount, pointIndex, flag, trimmedExponent)

          coefficient <- PtrPeeker.fixed do
            foldl' (\l r -> l * 10000 + fromIntegral r) 0
              <$> replicateM componentsAmount PtrPeeker.beSignedInt2

          pure
            let byCoefficient coefficient =
                  let exponent = (fromIntegral pointIndex + 1 - componentsAmount) * 4
                      scientific = Scientific.scientific coefficient exponent
                   in if prec == 0 && sc == 0
                        then Right (ScientificNumeric scientific)
                        else
                          if Scientific.validateNumericPrecisionScale prec sc scientific
                            then Right (ScientificNumeric scientific)
                            else
                              Left
                                DecodingError
                                  { location = ["precision-scale-validation"],
                                    reason =
                                      UnexpectedValueDecodingErrorReason
                                        (TextBuilder.toText ("value within precision=" <> TextBuilder.decimal prec <> ", scale=" <> TextBuilder.decimal sc))
                                        (Text.pack (Scientific.formatScientific Scientific.Fixed Nothing scientific))
                                  }
             in case flag of
                  0x0000 -> byCoefficient coefficient
                  0x4000 -> byCoefficient (negate coefficient)
                  0xC000 -> Right NanNumeric
                  0xD000 -> Right PosInfinityNumeric
                  0xF000 -> Right NegInfinityNumeric
                  _ ->
                    Left
                      DecodingError
                        { location = ["flag"],
                          reason =
                            UnexpectedValueDecodingErrorReason
                              "0x0000, 0x4000, 0xC000, 0xD000, or 0xF000"
                              (Text.toUpper (TextBuilder.toText (TextBuilder.prefixedHexadecimal flag)))
                        }

  textualEncoder =
    let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
        sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
     in \case
          ScientificNumeric scientific ->
            if sc == 0 && prec /= 0
              then TextBuilder.string (Scientific.formatScientific Scientific.Fixed (Just 0) scientific)
              else TextBuilder.string (Scientific.formatScientific Scientific.Fixed Nothing scientific)
          NanNumeric ->
            "NaN"
          NegInfinityNumeric ->
            "-Infinity"
          PosInfinityNumeric ->
            "Infinity"

  textualDecoder =
    let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
        sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
     in asum
          [ if prec == 0 && sc == 0
              then ScientificNumeric <$> Attoparsec.scientific
              else do
                scientific <- Attoparsec.scientific
                if Scientific.validateNumericPrecisionScale prec sc scientific
                  then pure (ScientificNumeric scientific)
                  else fail ("Value does not satisfy the \"precision=" <> show prec <> ", scale=" <> show sc <> "\" constraints: " <> show scientific),
            NanNumeric <$ Attoparsec.string "NaN",
            NegInfinityNumeric <$ Attoparsec.string "-Infinity",
            NegInfinityNumeric <$ Attoparsec.string "-inf",
            PosInfinityNumeric <$ Attoparsec.string "Infinity",
            PosInfinityNumeric <$ Attoparsec.string "inf"
          ]

-- | Mapping to @numrange@ type.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsRangeElement (Numeric precision scale) where
  rangeTypeName = Tagged "numrange"
  rangeBaseOid = Tagged (Just 3906)
  rangeArrayOid = Tagged (Just 3907)

-- | Mapping to @nummultirange@ type.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsMultirangeElement (Numeric precision scale) where
  multirangeTypeName = Tagged "nummultirange"
  multirangeBaseOid = Tagged (Just 4532)
  multirangeArrayOid = Tagged (Just 6151)

-- | Checks if the 'Numeric' value is 'Infinity'.
isPosInfinity :: Numeric precision scale -> Bool
isPosInfinity = \case
  PosInfinityNumeric -> True
  _ -> False

-- | Checks if the 'Numeric' value is '-Infinity'.
isNegInfinity :: Numeric precision scale -> Bool
isNegInfinity = \case
  NegInfinityNumeric -> True
  _ -> False

-- | Checks if the 'Numeric' value is 'NaN'.
isNaN :: Numeric precision scale -> Bool
isNaN = \case
  NanNumeric -> True
  _ -> False

-- | Represents '+Infinity' value for 'Numeric' type.
posInfinity :: Numeric precision scale
posInfinity = PosInfinityNumeric

-- | Represents '-Infinity' value for 'Numeric' type.
negInfinity :: Numeric precision scale
negInfinity = NegInfinityNumeric

-- | Represents 'NaN' value for 'Numeric' type.
nan :: Numeric precision scale
nan = NanNumeric

-- | Normalizes a 'Numeric' value to 'Scientific.Scientific'.
--
-- Special values like 'NaN', 'Infinity', and '-Infinity' are normalized to 0.
normalizeToScientific :: Numeric precision scale -> Scientific.Scientific
normalizeToScientific = \case
  ScientificNumeric s -> s
  NanNumeric -> 0
  PosInfinityNumeric -> 0
  NegInfinityNumeric -> 0

-- | Normalizes a 'Scientific.Scientific' value to 'Numeric' with given precision and scale.
--
-- Clamps to the specified precision and scale.
--
-- If both precision and scale are 0, clamps to the PostgreSQL numeric limits.
normalizeFromScientific ::
  forall precision scale.
  (TypeLits.KnownNat precision, TypeLits.KnownNat scale) =>
  Scientific.Scientific ->
  Numeric precision scale
normalizeFromScientific s =
  let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
      sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
   in if prec == 0 && sc == 0
        then ScientificNumeric (Scientific.clampToPostgresNumericLimits s)
        else
          if sc > prec
            then NanNumeric -- Invalid configuration: scale cannot exceed precision
            else ScientificNumeric (Scientific.clampToPrecisionAndScale prec sc s)

-- | Converts a 'Numeric' value to 'Scientific.Scientific' if possible.
-- Returns 'Nothing' for special values like 'NanNumeric', 'PosInfinityNumeric', and 'NegInfinityNumeric'.
refineToScientific :: Numeric precision scale -> Maybe Scientific.Scientific
refineToScientific = \case
  ScientificNumeric s -> Just s
  _ -> Nothing

-- | Converts a 'Scientific.Scientific' value to 'Numeric' with validation.
-- Returns 'Nothing' if the value does not fit within the specified precision and scale constraints.
refineFromScientific :: forall precision scale. (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => Scientific.Scientific -> Maybe (Numeric precision scale)
refineFromScientific s =
  let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
      sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
   in if prec == 0 && sc == 0
        then
          -- Validate against PostgreSQL limits for arbitrary precision numeric
          if Scientific.validatePostgresNumericLimits s
            then Just (ScientificNumeric s)
            else Nothing
        else
          if sc > prec
            then Nothing -- Invalid configuration: scale cannot exceed precision
            else
              if Scientific.validateNumericPrecisionScale prec sc s
                then Just (ScientificNumeric s)
                else Nothing
