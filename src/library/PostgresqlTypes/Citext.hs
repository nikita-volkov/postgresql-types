module PostgresqlTypes.Citext
  ( Citext,

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

-- | PostgreSQL @citext@ type. Case-insensitive variable-length character string.
-- Requires the @citext@ extension to be installed in PostgreSQL.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/18/citext.html).
--
-- The value is stored as UTF-8 text, identical to @text@. Case-insensitivity
-- is a property of comparisons, not of storage. NUL characters are not allowed.
newtype Citext = Citext Text.Text
  deriving newtype (Eq, Ord, Hashable)
  deriving (Show, Read, IsString) via (ViaIsScalar Citext)

instance Arbitrary Citext where
  arbitrary =
    Citext <$> do
      charList <- QuickCheck.listOf do
        QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
      pure (Text.pack charList)
  shrink (Citext base) =
    Citext . Text.pack <$> shrink (Text.unpack base)

instance IsScalar Citext where
  schemaName = Tagged Nothing
  typeName = Tagged "citext"
  baseOid = Tagged Nothing
  arrayOid = Tagged Nothing
  typeParams = Tagged []
  binaryEncoder (Citext base) = Write.textUtf8 base
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
      Right base -> pure (Right (Citext base))
  textualEncoder (Citext base) = TextBuilder.text base
  textualDecoder = Citext <$> Attoparsec.takeText

-- * Accessors

-- | Extract the underlying 'Data.Text.Text' value.
toText :: Citext -> Text.Text
toText (Citext t) = t

-- * Constructors

-- | Construct a PostgreSQL 'Citext' from a 'Data.Text.Text' value.
--
-- Strips null characters to ensure PostgreSQL compatibility.
normalizeFromText :: Text.Text -> Citext
normalizeFromText = Citext . Text.replace "\NUL" ""

-- | Construct a PostgreSQL 'Citext' from a 'Data.Text.Text' value.
--
-- Returns 'Nothing' if the text contains null characters (not supported by PostgreSQL).
refineFromText :: Text.Text -> Maybe Citext
refineFromText text =
  if Text.elem '\NUL' text
    then Nothing
    else Just (Citext text)
