module PostgresqlTypes.Types.Text (Text) where

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
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-character.html).
newtype Text = Text Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsScalar Text)

instance Arbitrary Text where
  arbitrary =
    Text <$> do
      charList <- QuickCheck.listOf do
        QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
      pure (Text.pack charList)
  shrink (Text base) =
    Text . Text.pack <$> shrink (Text.unpack base)

instance IsScalar Text where
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

-- | Conversion from Haskell 'Data.Text.Text'.
-- Fails if the text contains null characters (not supported by PostgreSQL).
instance IsSome Text.Text Text where
  to (Text t) = t
  maybeFrom text =
    if Text.elem '\NUL' text
      then Nothing
      else Just (Text text)

-- | Total conversion from Haskell 'Data.Text.Text'.
-- Strips null characters to ensure PostgreSQL compatibility.
instance IsMany Text.Text Text where
  onfrom = Text . Text.replace "\NUL" ""
