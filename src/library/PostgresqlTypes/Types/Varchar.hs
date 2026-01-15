module PostgresqlTypes.Types.Varchar (Varchar) where

import qualified Data.Attoparsec.Text as Attoparsec
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (Text)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @varchar@ type. Variable-length character string with limit.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-character.html).
newtype Varchar = Varchar Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Varchar)

instance Arbitrary Varchar where
  arbitrary =
    Varchar <$> do
      charList <- QuickCheck.listOf do
        QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
      pure (Text.pack charList)
  shrink (Varchar base) =
    Varchar . Text.pack <$> shrink (Text.unpack base)

instance IsStandardType Varchar where
  typeIdsOf =
    TypeIdsOf
      { name = "varchar",
        stableBaseOid = Just 1043,
        stableArrayOid = Just 1015
      }
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
        Right (Varchar base)
  textualEncoder (Varchar base) = TextBuilder.text base
  textualDecoder = Varchar <$> Attoparsec.takeText

instance IsSome Text.Text Varchar where
  to = coerce
  maybeFrom text =
    if Text.elem '\NUL' text
      then Nothing
      else Just (Varchar text)

instance IsMany Text.Text Varchar where
  onfrom = Varchar . Text.replace "\NUL" ""
