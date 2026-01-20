-- | Utilities for Scientific.
module PostgresqlTypes.Types.Numeric.Scientific where

import qualified Data.Scientific as Scientific
import PostgresqlTypes.Prelude
import qualified PostgresqlTypes.Types.Numeric.Integer as Integer

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
            let totalDigitsInCoeff = Integer.countDigits absCoeff
             in -- If the coefficient has fewer digits than scale, there are leading zeros
                -- e.g., for 0.0012 with scale=4, coeff=12, scale=4, but totalDigits=2
                -- The significant digits are just the digits in coeff
                -- For 123.45 with scale=2, coeff=12345, totalDigits=5, sigDigits=5
                totalDigitsInCoeff
   in sigDigits <= prec

-- | Validates that a Scientific value fits within PostgreSQL's numeric type limits:
-- - Up to 131072 digits before decimal point
-- - Up to 16383 digits after decimal point
validatePostgresNumericLimits :: Scientific.Scientific -> Bool
validatePostgresNumericLimits s =
  let coeff = Scientific.coefficient s
      exp = Scientific.base10Exponent s
      absCoeff = abs coeff

      -- If coefficient is 0, it's always valid
      -- Otherwise, calculate digits before and after decimal point
      (digitsBefore, digitsAfter) =
        if absCoeff == 0
          then (0, 0)
          else
            let totalDigits = Integer.countDigits absCoeff
             in -- Exponent tells us where the decimal point is
                -- Positive exponent means more digits before decimal
                -- Negative exponent means digits after decimal
                if exp >= 0
                  then (totalDigits + exp, 0)
                  else
                    let absExp = abs exp
                     in if absExp >= totalDigits
                          then (0, absExp) -- All digits are after decimal (e.g., 0.00123)
                          else (totalDigits - absExp, absExp) -- Some before, some after
   in digitsBefore <= 131072 && digitsAfter <= 16383

-- | Clamp a Scientific value to fit within precision and scale constraints.
-- Rounds the value to the specified scale and clamps the magnitude to fit precision.
clampToPrecisionAndScale :: Int -> Int -> Scientific.Scientific -> Scientific.Scientific
clampToPrecisionAndScale prec sc s =
  let -- First, round to the correct scale
      rounded = roundToScale sc s
      coeff = Scientific.coefficient rounded
      -- Calculate maximum absolute coefficient for given precision
      -- When scaled by 10^sc, max is 10^prec - 1
      maxCoeff = 10 ^ prec - 1
   in if abs coeff > maxCoeff
        then Scientific.scientific (if coeff < 0 then negate maxCoeff else maxCoeff) (negate sc)
        else rounded

-- | Clamp a Scientific value to fit within PostgreSQL numeric limits:
-- - Up to 131072 digits before decimal point
-- - Up to 16383 digits after decimal point
clampToPostgresNumericLimits :: Scientific.Scientific -> Scientific.Scientific
clampToPostgresNumericLimits s =
  let exp = Scientific.base10Exponent s

      -- First clamp the scale (digits after decimal) to 16383
      maxScale = 16383
      clampedToScale =
        if exp < negate maxScale
          then roundToScale maxScale s
          else s

      -- Now clamp the integer part to 131072 digits
      coeff' = Scientific.coefficient clampedToScale
      exp' = Scientific.base10Exponent clampedToScale
      absCoeff' = abs coeff'

      maxDigitsBefore = 131072
   in if absCoeff' == 0
        then clampedToScale
        else
          let totalDigits = Integer.countDigits absCoeff'
              -- Calculate how many digits are before decimal point
              digitsBefore =
                if exp' >= 0
                  then totalDigits + exp'
                  else max 0 (totalDigits + exp')
           in if digitsBefore > maxDigitsBefore
                then
                  -- Need to clamp: reduce coefficient to fit maxDigitsBefore
                  -- Calculate the maximum coefficient that fits
                  let digitsAfter = if exp' < 0 then min maxScale (abs exp') else 0
                      maxTotalDigits = maxDigitsBefore + digitsAfter
                      excessDigits = totalDigits - maxTotalDigits
                      clampedCoeff = absCoeff' `div` (10 ^ excessDigits)
                      signedClampedCoeff = if coeff' < 0 then negate clampedCoeff else clampedCoeff
                   in Scientific.scientific signedClampedCoeff exp'
                else clampedToScale

-- | Round a Scientific value to a specific scale (number of decimal places)
-- Uses round ties away from zero (PostgreSQL numeric rounding behavior)
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
              -- Round ties away from zero (PostgreSQL behavior)
              -- If remainder * 2 >= divisor, round away from zero
              roundedQuotient =
                if absRemainder * 2 >= divisor
                  then quotient + 1
                  else quotient
              -- Apply sign
              finalQuotient = if coeff < 0 then negate roundedQuotient else roundedQuotient
           in Scientific.scientific finalQuotient targetExp
