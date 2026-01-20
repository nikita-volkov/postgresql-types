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
      absCoeff = abs coeff

      -- Zero fits into any NUMERIC(precision, scale)
      in if absCoeff == 0
           then True
           else
             let totalDigits = Integer.countDigits absCoeff

                 -- Compute digits before and after the decimal point, following the same
                 -- approach as in 'validatePostgresNumericLimits'.
                 (digitsBefore, digitsAfterRaw) =
                   if exp >= 0
                     then (totalDigits + exp, 0)
                     else
                       let absExp = abs exp
                        in if absExp >= totalDigits
                             then (0, absExp)
                             else (totalDigits - absExp, absExp)

                 -- PostgreSQL's precision is the total number of decimal digits.
                 -- The declared scale limits the number of fractional digits; any extra
                 -- fractional digits would be rounded or truncated to 'sc'.
                 digitsAfter = min digitsAfterRaw sc
                 maxIntegerDigits = prec - sc
              in digitsBefore <= maxIntegerDigits
                 && digitsBefore + digitsAfter <= prec
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
