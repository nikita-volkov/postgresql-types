module PostgresqlTypes.Types.Varchar (Varchar) where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified GHC.TypeLits as TypeLits
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (Text)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @varchar(n)@ type. Variable-length character string with limit.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-character.html).
--
-- The type parameter @numChars@ specifies the static maximum length of the character string.
-- Character strings up to this length can be represented by this type.
data Varchar (numChars :: TypeLits.Nat) = Varchar Text.Text
  deriving stock (Eq, Ord)
  deriving (Show) via (ViaIsStandardType (Varchar numChars))

instance (TypeLits.KnownNat numChars) => Arbitrary (Varchar numChars) where
  arbitrary = do
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
    len <- QuickCheck.chooseInt (0, maxLen)
    charList <- QuickCheck.vectorOf len do
      QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
    pure (Varchar (Text.pack charList))
  shrink (Varchar base) =
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
        shrunk = Text.pack <$> shrink (Text.unpack base)
     in [Varchar txt | txt <- shrunk, Text.length txt <= maxLen]

instance (TypeLits.KnownNat numChars) => IsStandardType (Varchar numChars) where
  typeName = Tagged "varchar"
  baseOid = Tagged (Just 1043)
  arrayOid = Tagged (Just 1015)
  typeParams =
    Tagged [Text.pack (show (TypeLits.natVal (Proxy @numChars)))]
  binaryEncoder (Varchar base) = Write.textUtf8 base
  binaryDecoder = do
    bytes <- PtrPeeker.remainderAsByteString
    pure case Text.Encoding.decodeUtf8' bytes of
      Left e ->
        Left
          ( DecodingError
              { location = [],
                reason =
                  ParsingDecodingErrorReason
                    (fromString (show e))
                    bytes
              }
          )
      Right base ->
        let len = Text.length base
            maxLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
         in if len <= maxLen
              then Right (Varchar base)
              else
                Left
                  ( DecodingError
                      { location = ["Varchar"],
                        reason =
                          UnsupportedValueDecodingErrorReason
                            ("Varchar string length " <> Text.pack (show len) <> " exceeds maximum " <> Text.pack (show maxLen))
                            (Text.take 100 base)
                      }
                  )
  textualEncoder (Varchar base) = TextBuilder.text base
  textualDecoder = do
    text <- Attoparsec.takeText
    let len = Text.length text
        maxLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
    if len <= maxLen
      then pure (Varchar text)
      else fail ("Varchar string length " <> show len <> " exceeds maximum " <> show maxLen)

instance (TypeLits.KnownNat numChars) => IsSome Text.Text (Varchar numChars) where
  to (Varchar text) = text
  maybeFrom text =
    let len = Text.length text
        maxLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
     in if Text.elem '\NUL' text
          then Nothing
          else
            if len <= maxLen
              then Just (Varchar text)
              else Nothing

instance (TypeLits.KnownNat numChars) => IsMany Text.Text (Varchar numChars) where
  onfrom text =
    let maxLen = fromIntegral (TypeLits.natVal (Proxy @numChars))
        cleanedText = Text.replace "\NUL" "" text
        truncatedText = Text.take maxLen cleanedText
     in Varchar truncatedText
