module PostgresqlTypes.Types.Bpchar (Bpchar) where

import qualified Data.Attoparsec.Text as Attoparsec
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

-- | PostgreSQL @bpchar(n)@ type. Fixed-length character string.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-character.html).
--
-- The type parameter @numChars@ specifies the static length of the character string.
-- Only character strings with exactly this length can be represented by this type.
-- For the single-byte @bpchar@ type (not @bpchar(n)@), use @Bpchar 1@.
data Bpchar (numChars :: TypeLits.Nat) = Bpchar Text
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType (Bpchar numChars))

instance (TypeLits.KnownNat numChars) => Arbitrary (Bpchar numChars) where
  arbitrary = do
    let len = fromIntegral (TypeLits.natVal (Proxy @numChars))
    charList <- QuickCheck.vectorOf len do
      QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
    case maybeFrom charList of
      Nothing -> error "Arbitrary Bpchar: Generated string has incorrect length"
      Just char -> pure char

instance (TypeLits.KnownNat numChars) => IsStandardType (Bpchar numChars) where
  typeName = Tagged "bpchar"
  baseOid = Tagged (Just 1042)
  arrayOid = Tagged (Just 1014)

  typeParams =
    Tagged
      ( let len = TypeLits.natVal (Proxy @numChars)
         in if len == 1
              then [] -- char without length modifier
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

-- | Convert from a character string (as String) to a Bpchar.
-- The string must have exactly the length specified by the type parameter.
instance (TypeLits.KnownNat numChars) => IsSome String (Bpchar numChars) where
  to (Bpchar txt) = Text.unpack txt
  maybeFrom str =
    let len = length str
        expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
     in if len == expectedLen
          then Just (Bpchar (Text.pack str))
          else Nothing

-- | Direct conversion from String to Bpchar.
-- Truncates or pads with spaces to match the type-level length.
instance (TypeLits.KnownNat numChars) => IsMany String (Bpchar numChars) where
  onfrom str =
    let expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
        -- Truncate or pad with spaces to the expected length
        adjustedStr = take expectedLen (str ++ repeat ' ')
     in Bpchar (Text.pack adjustedStr)

-- | Convert from a character string (as Text) to a Bpchar.
--
-- This provides an efficient conversion from 'Data.Text.Text' to PostgreSQL @bpchar(n)@ type.
-- The text must have exactly the length specified by the type parameter.
--
-- This instance allows using Text for high-performance string operations
-- while maintaining compatibility with PostgreSQL's fixed-length character format.
instance (TypeLits.KnownNat numChars) => IsSome Text (Bpchar numChars) where
  to (Bpchar txt) = txt
  maybeFrom txt =
    let len = Text.length txt
        expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
     in if len == expectedLen
          then Just (Bpchar txt)
          else Nothing

-- | Direct conversion from Text to Bpchar.
--
-- This is a total conversion that truncates or pads with spaces to match the type-level length.
-- The text is efficiently stored in the PostgreSQL @bpchar(n)@ format.
instance (TypeLits.KnownNat numChars) => IsMany Text (Bpchar numChars) where
  onfrom txt =
    let expectedLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
        len = Text.length txt
        adjustedTxt =
          if len >= expectedLen
            then Text.take expectedLen txt
            else txt <> Text.replicate (expectedLen - len) " "
     in Bpchar adjustedTxt
