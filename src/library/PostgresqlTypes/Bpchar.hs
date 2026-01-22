module PostgresqlTypes.Bpchar
  ( Bpchar,

    -- * Accessors
    toText,
    toString,

    -- * Constructors
    refineFromText,
    normalizeFromText,
    refineFromString,
    normalizeFromString,
  )
where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.Hashable (Hashable (..))
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified GHC.TypeLits as TypeLits
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @bpchar(n)@, @char(n)@, or @character(n)@ type. Fixed-length, blank-padded character string.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-character.html).
--
-- The type parameter @numChars@ specifies the static length of the character string.
-- Only character strings with exactly this length can be represented by this type.
--
-- __Important:__ Do not confuse this with the quoted @\"char\"@ type, which is a special
-- single-byte internal type used in PostgreSQL system catalogs. The quoted @\"char\"@ type
-- is represented by 'PostgresqlTypes.Char.Char', not by @Bpchar 1@.
--
-- * @Bpchar n@ represents @bpchar(n)@, @char(n)@, or @character(n)@ — fixed-length, blank-padded strings
-- * 'PostgresqlTypes.Char.Char' represents @\"char\"@ (quoted) — single-byte internal type
--
-- For example, @char(1)@ in SQL is @Bpchar 1@ in Haskell, while @\"char\"@ in SQL is
-- 'PostgresqlTypes.Char.Char' in Haskell. These are completely different types in PostgreSQL.
data Bpchar (numChars :: TypeLits.Nat) = Bpchar Text
  deriving stock (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar (Bpchar numChars))

instance (TypeLits.KnownNat numChars) => Arbitrary (Bpchar numChars) where
  arbitrary = do
    let len = fromIntegral (TypeLits.natVal (Proxy @numChars))
    charList <- QuickCheck.vectorOf len do
      QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
    case refineFromString charList of
      Nothing -> error "Arbitrary Bpchar: Generated string has incorrect length"
      Just char -> pure char

instance Hashable (Bpchar numChars) where
  hashWithSalt salt (Bpchar txt) = hashWithSalt salt txt

instance (TypeLits.KnownNat numChars) => IsScalar (Bpchar numChars) where
  schemaName = Tagged Nothing
  typeName = Tagged "bpchar"
  baseOid = Tagged (Just 1042)
  arrayOid = Tagged (Just 1014)

  typeParams =
    Tagged
      ( let len = TypeLits.natVal (Proxy @numChars)
         in if len == 1
              then [] -- PostgreSQL often displays bpchar(1) / char(1) as just "char" (no length modifier)
              else [Text.pack (show len)] -- bpchar(n)
      )
  binaryEncoder (Bpchar txt) =
    -- PostgreSQL bpchar(n) is stored blank-padded to exactly n characters
    let expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
        len = Text.length txt
        paddedTxt =
          if len >= expectedLen
            then Text.take expectedLen txt
            else txt <> Text.replicate (expectedLen - len) " "
     in Write.textUtf8 paddedTxt
  binaryDecoder = do
    bytes <- PtrPeeker.remainderAsByteString
    pure case Text.Encoding.decodeUtf8' bytes of
      Left e ->
        Left
          ( DecodingError
              { location = ["Bpchar"],
                reason =
                  ParsingDecodingErrorReason
                    (fromString (show e))
                    bytes
              }
          )
      Right txt ->
        let len = Text.length txt
            expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
            -- PostgreSQL bpchar(n) may return values with trailing spaces trimmed.
            -- We need to pad them back to the expected length.
            paddedTxt =
              if len >= expectedLen
                then Text.take expectedLen txt
                else txt <> Text.replicate (expectedLen - len) " "
         in Right (Bpchar paddedTxt)
  textualEncoder (Bpchar txt) =
    -- PostgreSQL bpchar(n) is stored blank-padded to exactly n characters
    let expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
        len = Text.length txt
        paddedTxt =
          if len >= expectedLen
            then Text.take expectedLen txt
            else txt <> Text.replicate (expectedLen - len) " "
     in TextBuilder.text paddedTxt
  textualDecoder = do
    txt <- Attoparsec.takeText
    let len = Text.length txt
        expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
        -- PostgreSQL bpchar(n) may return values with trailing spaces trimmed.
        -- We need to pad them back to the expected length.
        paddedTxt =
          if len >= expectedLen
            then Text.take expectedLen txt
            else txt <> Text.replicate (expectedLen - len) " "
    pure (Bpchar paddedTxt)

-- * Accessors

-- | Extract the underlying 'Text' value.
toText :: forall numChars. (TypeLits.KnownNat numChars) => Bpchar numChars -> Text
toText (Bpchar txt) = txt

-- | Convert the Bpchar to a String.
toString :: forall numChars. (TypeLits.KnownNat numChars) => Bpchar numChars -> String
toString (Bpchar txt) = Text.unpack txt

-- * Constructors

-- | Construct a PostgreSQL 'Bpchar' from 'Text' with validation.
-- Returns 'Nothing' if the text length doesn't exactly match the expected length.
refineFromText :: forall numChars. (TypeLits.KnownNat numChars) => Text -> Maybe (Bpchar numChars)
refineFromText txt =
  let len = Text.length txt
      expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
   in if len == expectedLen
        then Just (Bpchar txt)
        else Nothing

-- | Construct a PostgreSQL 'Bpchar' from 'Text'.
-- Truncates or pads with spaces to match the type-level length.
normalizeFromText :: forall numChars. (TypeLits.KnownNat numChars) => Text -> Bpchar numChars
normalizeFromText txt =
  let expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
      len = Text.length txt
      adjustedTxt =
        if len >= expectedLen
          then Text.take expectedLen txt
          else txt <> Text.replicate (expectedLen - len) " "
   in Bpchar adjustedTxt

-- | Construct a PostgreSQL 'Bpchar' from 'String' with validation.
-- Returns 'Nothing' if the string length doesn't exactly match the expected length.
refineFromString :: forall numChars. (TypeLits.KnownNat numChars) => String -> Maybe (Bpchar numChars)
refineFromString str =
  let len = length str
      expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
   in if len == expectedLen
        then Just (Bpchar (Text.pack str))
        else Nothing

-- | Construct a PostgreSQL 'Bpchar' from 'String'.
-- Truncates or pads with spaces to match the type-level length.
normalizeFromString :: forall numChars. (TypeLits.KnownNat numChars) => String -> Bpchar numChars
normalizeFromString str =
  let expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
      -- Truncate or pad with spaces to the expected length
      adjustedStr = take expectedLen (str ++ repeat ' ')
   in Bpchar (Text.pack adjustedStr)
