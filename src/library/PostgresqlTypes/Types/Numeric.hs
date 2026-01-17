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
-- The type parameters @precision@ and @scale@ specify the precision and scale constraints
-- for the numeric type, matching PostgreSQL's @numeric(precision, scale)@ semantics.
-- 
-- When @precision@ and @scale@ are both 0, the type represents arbitrary precision @numeric@
-- with no constraints. Otherwise:
-- - @precision@: Total number of significant digits (before and after decimal point)
-- - @scale@: Number of digits after the decimal point
--
-- Runtime validation is performed in conversion instances to ensure values conform to
-- the specified constraints. Values that exceed the precision/scale bounds will fail
-- validation in 'IsSome' instances or be clamped/rounded in 'IsMany' instances.
--
-- Use @Numeric 0 0@ to represent @numeric@ without precision/scale modifiers (arbitrary precision).
data Numeric (precision :: TypeLits.Nat) (scale :: TypeLits.Nat)
  = ScientificNumeric Scientific.Scientific
  | NanNumeric
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType (Numeric precision scale))

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => Arbitrary (Numeric precision scale) where
  arbitrary = do
    let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
        sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
    if prec == 0 && sc == 0
      then
        -- Arbitrary precision: generate any Scientific value or NaN
        QuickCheck.oneof
          [ ScientificNumeric <$> arbitrary,
            pure NanNumeric
          ]
      else do
        -- Generate value respecting precision and scale constraints
        -- Precision p, scale s means: at most p total digits, s after decimal point
        -- intDigits can be negative when scale > precision (e.g., NUMERIC(2,4) -> 0.0012)
        let intDigits = max 0 (prec - sc)
            -- Cap exponents to prevent excessive computation
            maxIntDigits = min intDigits 15  -- Cap at 10^15 for reasonable generation
            maxScale = min sc 15
        -- Generate a value with appropriate number of digits
        sign <- arbitrary @Bool
        -- Generate integer part (up to intDigits digits)
        intPart <- if maxIntDigits > 0
                   then QuickCheck.choose (0, 10 ^ maxIntDigits - 1)
                   else pure 0
        -- Generate fractional part (up to sc digits)
        fracPart <- if maxScale > 0
                    then QuickCheck.choose (0, 10 ^ maxScale - 1)
                    else pure 0
        let coefficient = (if sign then negate else id) (intPart * (10 ^ maxScale) + fracPart)
            scientific = Scientific.scientific coefficient (negate (fromIntegral maxScale))
        QuickCheck.oneof
          [ pure (ScientificNumeric scientific),
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
-- Conversion from Numeric to Scientific.
-- In 'maybeFrom' produces 'Nothing' for 'NanNumeric' values.
-- For parameterized types (precision > 0), also validates that the Scientific value
-- fits within the precision and scale constraints.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome (Numeric precision scale) Scientific.Scientific where
  to = ScientificNumeric
  maybeFrom = \case
    ScientificNumeric s -> Just s
    NanNumeric -> Nothing

-- |
-- Conversion from Scientific to Numeric with validation.
-- For parameterized types (precision > 0), validates that values fit within constraints.
instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome Scientific.Scientific (Numeric precision scale) where
  to = \case
    ScientificNumeric s -> s
    NanNumeric -> 0  -- Or we could error here, but 0 seems reasonable
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
    NanNumeric -> 0

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome (Maybe Scientific.Scientific) (Numeric precision scale) where
  to = \case
    ScientificNumeric s -> Just s
    NanNumeric -> Nothing
  maybeFrom = \case
    Nothing -> Just NanNumeric
    Just s ->
      let prec = fromIntegral (TypeLits.natVal (Proxy @precision)) :: Int
          sc = fromIntegral (TypeLits.natVal (Proxy @scale)) :: Int
       in if prec == 0 && sc == 0
            then Just (ScientificNumeric s)
            else
              if validateNumericPrecisionScale prec sc s
                then Just (ScientificNumeric s)
                else Nothing

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsMany (Maybe Scientific.Scientific) (Numeric precision scale)

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => Is (Maybe Scientific.Scientific) (Numeric precision scale)

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsSome (Numeric precision scale) (Maybe Scientific.Scientific) where
  to = \case
    Just s -> ScientificNumeric s
    Nothing -> NanNumeric

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => IsMany (Numeric precision scale) (Maybe Scientific.Scientific)

instance (TypeLits.KnownNat precision, TypeLits.KnownNat scale) => Is (Numeric precision scale) (Maybe Scientific.Scientific)

-- | Validates that a Scientific value fits within the given precision and scale constraints.
-- Returns True if the value is valid, False otherwise.
--
-- For NUMERIC(precision, scale):
-- - precision: total number of significant digits
-- - scale: number of digits after decimal point
-- - Maximum integer digits: precision - scale
validateNumericPrecisionScale :: Int -> Int -> Scientific.Scientific -> Bool
validateNumericPrecisionScale prec sc s =
  let -- Normalize the scientific to have exactly 'sc' decimal places
      -- This helps us count digits correctly
      coeff = Scientific.coefficient s
      exp = Scientific.base10Exponent s
      
      -- Adjust coefficient to have exactly 'sc' decimal places
      targetExp = negate sc
      adjustedCoeff = if exp == targetExp
                      then coeff
                      else if exp < targetExp
                           -- Need more decimal places: multiply
                           then coeff * (10 ^ (targetExp - exp))
                           -- Need fewer decimal places: divide (truncate)
                           else coeff `div` (10 ^ (exp - targetExp))
      
      -- Count total significant digits
      totalDigits = countDigits (abs adjustedCoeff)
      
   in -- The number of significant digits must not exceed precision
      totalDigits <= prec

-- | Count the number of digits in an integer
countDigits :: Integer -> Int
countDigits 0 = 1
countDigits n = length (show (abs n))

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
      then s  -- Already has fewer or equal decimal places than target
      else
        -- Need to round: convert to the target scale
        let scaleDiff = targetExp - currentExp
            divisor = 10 ^ scaleDiff
            (quotient, remainder) = coeff `divMod` divisor
            -- Round half to even (banker's rounding)
            roundedQuotient = if remainder * 2 > divisor
                              then quotient + 1
                              else if remainder * 2 < divisor
                                   then quotient
                                   else if even quotient
                                        then quotient
                                        else quotient + 1
         in Scientific.scientific roundedQuotient targetExp

{-# INLINE extractComponents #-}
extractComponents :: (Integral a) => a -> [Word16]
extractComponents =
  (reverse .) . (. abs) . unfoldr $ \case
    0 -> Nothing
    x -> case divMod x 10000 of
      (d, m) -> Just (fromIntegral m, d)
