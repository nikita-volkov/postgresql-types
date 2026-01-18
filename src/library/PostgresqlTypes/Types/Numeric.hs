{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

-- Reference implementation for numeric type in PostgreSQL:
--
-- - Sign flags: https://github.com/postgres/postgres/blob/6bca4b50d000e840cad17a9dd6cb46785fb2cedb/src/backend/utils/adt/numeric.c#L201-L203
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
                        -- intDigits can be negative when scale > precision (e.g., NUMERIC(2,4) -> 0.0012)

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
                          if validateNumericPrecisionScale prec sc scientific
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
                if validateNumericPrecisionScale prec sc scientific
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

-- |
-- Conversion from Numeric to Scientific.
-- In 'to' extracts Scientific from Numeric, producing '0' for 'NanNumeric'.
-- In 'maybeFrom' wraps Scientific in Numeric with validation for parameterized types.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome (Numeric precision scale) Scientific.Scientific where
  to s =
    let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
        sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
     in if prec == 0 && sc == 0
          then ScientificNumeric s
          else
            if validateNumericPrecisionScale prec sc s
              then ScientificNumeric s
              else error "Scientific value does not fit within Numeric precision/scale constraints"
  maybeFrom = \case
    ScientificNumeric s -> Just s
    _ -> Nothing

-- |
-- Conversion from Scientific to Numeric.
-- In 'to' extracts Scientific from Numeric, producing '0' for 'NanNumeric'.
-- In 'maybeFrom' wraps Scientific in Numeric with validation for parameterized types.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome Scientific.Scientific (Numeric precision scale) where
  to = \case
    ScientificNumeric s -> s
    NanNumeric -> 0
  maybeFrom s =
    let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
        sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
     in if prec == 0 && sc == 0
          then Just (ScientificNumeric s)
          else
            if validateNumericPrecisionScale prec sc s
              then Just (ScientificNumeric s)
              else Nothing

-- |
-- Conversion from Scientific to Numeric with clamping.
-- Treats 'NanNumeric' values as @0@.
-- For parameterized types (precision > 0), clamps values to fit within constraints.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsMany Scientific.Scientific (Numeric precision scale) where
  onfrom s =
    let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
        sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
     in if prec == 0 && sc == 0
          then ScientificNumeric s
          else ScientificNumeric (clampNumericValue prec sc s)

-- | Treats 'NanNumeric' values as @0@.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsMany (Numeric precision scale) Scientific.Scientific where
  onfrom = \case
    ScientificNumeric s -> s
    _ -> 0

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome (Maybe Scientific.Scientific) (Numeric precision scale) where
  to = \case
    ScientificNumeric s -> Just s
    _ -> Nothing
  maybeFrom = \case
    Nothing -> Nothing
    Just s ->
      let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
          sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
       in if prec == 0 && sc == 0
            then Just (ScientificNumeric s)
            else
              if validateNumericPrecisionScale prec sc s
                then Just (ScientificNumeric s)
                else Nothing

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsMany (Maybe Scientific.Scientific) (Numeric precision scale) where
  onfrom = \case
    Just s -> ScientificNumeric s
    Nothing -> NanNumeric

-- | Validates that a Scientific value fits within the given precision and scale constraints.
-- Returns True if the value is valid, False otherwise.
--
-- For NUMERIC(precision, scale):
-- - precision: total number of significant digits
-- - scale: number of digits after decimal point
-- - Maximum integer digits: precision - scale
validateNumericPrecisionScale :: Int -> Int -> Scientific.Scientific -> Bool
validateNumericPrecisionScale prec sc s =
  let coeff = Scientific.coefficient s
      exp = Scientific.base10Exponent s

      -- We need to count significant digits
      -- Significant digits are all non-zero digits plus any zeros between them or after the first non-zero digit
      -- For a value like 123.45, that's 5 significant digits
      -- For a value like 0.0012, that's 2 significant digits (leading zeros don't count)
      -- For a value like 120, that's 3 significant digits (trailing zeros do count)

      -- First, normalize to scale
      targetExp = negate sc
      normalized =
        if exp >= targetExp
          then Scientific.scientific coeff exp
          else
            -- Need to round/truncate to the target scale
            let shift = targetExp - exp
                divisor = 10 ^ shift
             in Scientific.scientific (coeff `div` divisor) targetExp

      normalizedCoeff = Scientific.coefficient normalized

      -- Count significant digits: for a value normalized to scale digits after decimal point,
      -- significant digits are all digits in the coefficient (excluding leading zeros if coefficient < 10^scale)
      -- But we need to handle the case where abs(coeff) < 10^scale (leading zeros)
      absCoeff = abs normalizedCoeff

      -- If coefficient is 0, we have 1 significant digit
      sigDigits =
        if absCoeff == 0
          then 1
          else
            let totalDigitsInCoeff = countDigits absCoeff
             in -- If the coefficient has fewer digits than scale, there are leading zeros
                -- e.g., for 0.0012 with scale=4, coeff=12, scale=4, but totalDigits=2
                -- The significant digits are just the digits in coeff
                -- For 123.45 with scale=2, coeff=12345, totalDigits=5, sigDigits=5
                totalDigitsInCoeff
   in sigDigits <= prec

-- | Count the number of digits in an integer (more efficiently)
countDigits :: Integer -> Int
countDigits 0 = 1
countDigits n = go (abs n) 0
  where
    go 0 acc = acc
    go x acc = go (x `div` 10) (acc + 1)

-- | Clamp a Scientific value to fit within precision and scale constraints.
-- Rounds the value to the specified scale and clamps the magnitude to fit precision.
clampNumericValue :: Int -> Int -> Scientific.Scientific -> Scientific.Scientific
clampNumericValue prec sc s =
  let -- First, round to the correct scale
      rounded = roundToScale sc s
      coeff = Scientific.coefficient rounded
      -- Calculate maximum absolute coefficient for given precision
      -- When scaled by 10^sc, max is 10^prec - 1
      maxCoeff = 10 ^ prec - 1
   in if abs coeff > maxCoeff
        then Scientific.scientific (if coeff < 0 then negate maxCoeff else maxCoeff) (negate sc)
        else rounded

-- | Round a Scientific value to a specific scale (number of decimal places)
-- Uses proper rounding (banker's rounding / round half to even)
roundToScale :: Int -> Scientific.Scientific -> Scientific.Scientific
roundToScale sc s =
  let currentExp = Scientific.base10Exponent s
      targetExp = negate sc
      coeff = Scientific.coefficient s
   in if currentExp >= targetExp
        then s -- Already has fewer or equal decimal places than target
        else
          -- Need to round: convert to the target scale
          let scaleDiff = targetExp - currentExp
              divisor = 10 ^ scaleDiff
              (quotient, remainder) = abs coeff `divMod` divisor
              absRemainder = abs remainder
              -- Round half to even (banker's rounding)
              roundedQuotient =
                if absRemainder * 2 > divisor
                  then quotient + 1
                  else
                    if absRemainder * 2 < divisor
                      then quotient
                      else
                        if even quotient
                          then quotient
                          else quotient + 1
              -- Apply sign
              finalQuotient = if coeff < 0 then negate roundedQuotient else roundedQuotient
           in Scientific.scientific finalQuotient targetExp

{-# INLINE extractComponents #-}
extractComponents :: (Integral a) => a -> [Word16]
extractComponents =
  (reverse .) . (. abs) . unfoldr $ \case
    0 -> Nothing
    x -> case divMod x 10000 of
      (d, m) -> Just (fromIntegral m, d)
