module PostgresqlTypes.Text
  ( Text,

    -- * Accessors
    toText,

    -- * Constructors
    normalizeFromText,
    refineFromText,
  )
where

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

-- | PostgreSQL @text@ type. Variable-length character string with no null-characters.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/datatype-character.html).
newtype Text = Text Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show, Read, IsString) via (ViaIsScalar Text)

instance Arbitrary Text where
  arbitrary =
    Text <$> do
      charList <- QuickCheck.listOf do
        QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
      pure (Text.pack charList)
  shrink (Text base) =
    Text . Text.pack <$> shrink (Text.unpack base)

instance IsScalar Text where
  schemaName = Tagged Nothing
  typeName = Tagged "text"
  baseOid = Tagged (Just 25)
  arrayOid = Tagged (Just 1009)
  typeParams = Tagged []
  binaryEncoder (Text base) = Write.textUtf8 base
  binaryDecoder = do
    bytes <- PtrPeeker.remainderAsByteString
    case Text.Encoding.decodeUtf8' bytes of
      Left e ->
        pure
          ( Left
              ( DecodingError
                  { location = [],
                    reason =
                      ParsingDecodingErrorReason
                        (fromString (show e))
                        bytes
                  }
              )
          )
      Right base -> pure (Right (Text base))
  textualEncoder (Text base) = TextBuilder.text base
  textualDecoder = Text <$> Attoparsec.takeText

-- * Accessors

-- | Extract the underlying 'Data.Text.Text' value.
toText :: Text -> Text.Text
toText (Text t) = t

-- * Constructors

-- | Construct a PostgreSQL 'Text' from a 'Data.Text.Text' value.
--
-- Strips null characters to ensure PostgreSQL compatibility.
normalizeFromText :: Text.Text -> Text
normalizeFromText = Text . Text.replace "\NUL" ""

-- | Construct a PostgreSQL 'Text' from a 'Data.Text.Text' value.
--
-- Returns 'Nothing' if the text contains null characters (not supported by PostgreSQL).
refineFromText :: Text.Text -> Maybe Text
refineFromText text =
  if Text.elem '\NUL' text
    then Nothing
    else Just (Text text)
