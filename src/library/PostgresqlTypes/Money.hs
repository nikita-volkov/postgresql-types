module PostgresqlTypes.Money
  ( Money (..),

    -- * Accessors
    toInt64,

    -- * Constructors
    fromInt64,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Text as Text
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
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-money.html).
newtype Money = Money Int64
  deriving newtype (Eq, Ord, Hashable, Arbitrary)
  deriving (Show, Read, IsString) via (ViaIsScalar Money)

instance IsScalar Money where
  schemaName = Tagged Nothing
  typeName = Tagged "money"
  baseOid = Tagged (Just 790)
  arrayOid = Tagged (Just 791)
  typeParams = Tagged []
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
    -- Parse dollars (may include commas as thousands separators)
    dollarsText <- Attoparsec.takeWhile1 (\c -> isDigit c || c == ',')
    let dollarsStr = filter (/= ',') (Text.unpack dollarsText)
    dollars <- case readMaybe dollarsStr of
      Just n -> pure (n :: Int64)
      Nothing -> fail "Invalid dollar amount"
    _ <- Attoparsec.char '.'
    -- Parse exactly 2 cents digits
    centsDigit1 <- Attoparsec.digit
    centsDigit2 <- Attoparsec.digit
    let cents = fromIntegral (digitToInt centsDigit1 * 10 + digitToInt centsDigit2) :: Int64
        value = dollars * 100 + cents
    pure (Money (if isNegative then negate value else value))

-- * Accessors

-- | Extract the underlying 'Int64' value.
-- This represents the raw monetary value in the smallest currency unit
-- (e.g., cents for USD, where 123 represents $1.23).
toInt64 :: Money -> Int64
toInt64 (Money i) = i

-- * Constructors

-- | Construct a PostgreSQL 'Money' from an 'Int64' value.
fromInt64 :: Int64 -> Money
fromInt64 = Money
