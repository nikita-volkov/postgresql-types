module PostgresqlTypes.Types.Money (Money) where

import qualified Data.Attoparsec.Text as Attoparsec
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @money@ type. Currency amount.
--
-- The money type stores currency amounts as a 64-bit signed integer.
-- The scale (number of decimal places) is determined by the database's
-- currency locale settings, typically @2@ decimal places for most currencies.
--
-- Range: @-92233720368547758.08@ to @+92233720368547758.07@.
--
-- Note: The textual representation includes a currency symbol (e.g., @$1.23@) and currently does not support localization.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-money.html).
newtype Money = Money Int64
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsStandardType Money)

instance IsStandardType Money where
  typeName = Tagged "money"
  baseOid = Tagged 790
  arrayOid = Tagged 791
  binaryEncoder (Money x) = Write.bInt64 x
  binaryDecoder = PtrPeeker.fixed (Right . Money <$> PtrPeeker.beSignedInt8)
  textualEncoder (Money x) =
    -- Format as currency with 2 decimal places and $ symbol
    -- PostgreSQL's money type typically displays with currency symbol
    let isNegative = x < 0
        absValue = abs x
        dollars = quot absValue 100
        cents = rem absValue 100
        centsText =
          if cents < 10
            then "0" <> TextBuilder.decimal cents
            else TextBuilder.decimal cents
        signPrefix = if isNegative then "-" else ""
     in signPrefix <> "$" <> TextBuilder.decimal dollars <> "." <> centsText
  textualDecoder = do
    isNegative <- (True <$ Attoparsec.char '-') <|> pure False
    _ <- Attoparsec.char '$'
    dollars <- Attoparsec.decimal @Int64
    _ <- Attoparsec.char '.'
    cents <- Attoparsec.decimal @Int64
    let value = dollars * 100 + cents
    pure (Money (if isNegative then negate value else value))

-- | Direct conversion from 'Int64'.
-- This represents the raw monetary value in the smallest currency unit
-- (e.g., cents for USD, where 123 represents $1.23).
instance IsSome Int64 Money where
  to (Money i) = i
  maybeFrom = Just . Money

-- | Direct conversion from PostgreSQL Money to 'Int64'.
-- This extracts the raw monetary value as an integer.
instance IsSome Money Int64 where
  to i = Money i
  maybeFrom (Money i) = Just i

-- | Direct conversion from 'Int64'.
-- This is a total conversion as it always succeeds.
instance IsMany Int64 Money where
  onfrom = Money

-- | Direct conversion from PostgreSQL Money to 'Int64'.
-- This is a total conversion as it always succeeds.
instance IsMany Money Int64 where
  onfrom (Money i) = i

-- | Bidirectional conversion between 'Int64' and PostgreSQL Money.
instance Is Int64 Money

instance Is Money Int64
