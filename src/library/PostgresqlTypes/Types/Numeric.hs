module PostgresqlTypes.Types.Numeric
  ( Numeric,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified GHC.TypeLits as TypeLits
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
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
--
-- The type parameters @precision@ and @scale@ specify the static precision and scale of the numeric value.
-- Only numeric values conforming to these constraints can be represented by this type.
-- Use @Numeric 0 0@ to represent @numeric@ without precision/scale constraints (arbitrary precision).
data Numeric (precision :: TypeLits.Nat) (scale :: TypeLits.Nat)
  = ScientificNumeric Scientific.Scientific
  | NanNumeric
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType (Numeric precision scale))

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => Arbitrary (Numeric precision scale) where
  arbitrary =
    QuickCheck.oneof
      [ ScientificNumeric <$> arbitrary,
        pure NanNumeric
      ]

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsStandardType (Numeric precision scale) where
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
    (componentsAmount, pointIndex, signCode, _trimmedExponent) <- PtrPeeker.fixed do
      componentsAmount <- fromIntegral <$> PtrPeeker.beSignedInt2
      pointIndex <- PtrPeeker.beSignedInt2
      signCode <- PtrPeeker.beUnsignedInt2
      trimmedExponent <- PtrPeeker.beSignedInt2
      pure (componentsAmount, pointIndex, signCode, trimmedExponent)

    coefficient <- PtrPeeker.fixed do
      foldl' (\l r -> l * 10000 + fromIntegral r) 0
        <$> replicateM componentsAmount PtrPeeker.beSignedInt2

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
  textualDecoder =
    (NanNumeric <$ Attoparsec.string "NaN")
      <|> (ScientificNumeric <$> Attoparsec.scientific)

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

-- |
-- In 'maybeFrom' produces 'Nothing' for 'NanNumeric' values.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome (Numeric precision scale) Scientific.Scientific where
  to = ScientificNumeric
  maybeFrom = \case
    ScientificNumeric s -> Just s
    NanNumeric -> Nothing

-- | Treats 'NanNumeric' values as @0@.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsMany (Numeric precision scale) Scientific.Scientific where
  onfrom = \case
    ScientificNumeric s -> s
    NanNumeric -> 0

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome (Maybe Scientific.Scientific) (Numeric precision scale) where
  to = \case
    ScientificNumeric s -> Just s
    NanNumeric -> Nothing

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsMany (Maybe Scientific.Scientific) (Numeric precision scale)

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => Is (Maybe Scientific.Scientific) (Numeric precision scale)

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome (Numeric precision scale) (Maybe Scientific.Scientific) where
  to = \case
    Just s -> ScientificNumeric s
    Nothing -> NanNumeric

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsMany (Numeric precision scale) (Maybe Scientific.Scientific)

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => Is (Numeric precision scale) (Maybe Scientific.Scientific)

{-# INLINE extractComponents #-}
extractComponents :: (Integral a) => a -> [Word16]
extractComponents =
  (reverse .) . (. abs) . unfoldr $ \case
    0 -> Nothing
    x -> case divMod x 10000 of
      (d, m) -> Just (fromIntegral m, d)
