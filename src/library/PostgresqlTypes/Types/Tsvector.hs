module PostgresqlTypes.Types.Tsvector (Tsvector) where

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

-- | PostgreSQL @tsvector@ type. Text search document.
--
-- A tsvector value is a sorted list of distinct lexemes, which are words
-- that have been normalized to merge different variants of the same word.
--
-- The value is stored internally in PostgreSQL's normalized text format.
--
-- __Note__: The binary encoding/decoding currently uses a simplified implementation.
-- Full PostgreSQL wire protocol support for tsvector is complex and is a work in progress.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-textsearch.html).
newtype Tsvector = Tsvector Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Tsvector)

instance Arbitrary Tsvector where
  arbitrary = do
    -- Generate valid tsvector format: either empty or space-separated quoted lexemes
    numLexemes <- QuickCheck.chooseInt (0, 5)
    if numLexemes == 0
      then pure (Tsvector "")
      else do
        lexemes <- QuickCheck.vectorOf numLexemes genLexeme
        pure (Tsvector (Text.intercalate " " lexemes))
    where
      genLexeme = do
        -- Generate a simple word (lowercase letters)
        len <- QuickCheck.chooseInt (1, 10)
        word <- QuickCheck.vectorOf len (QuickCheck.elements ['a' .. 'z'])
        pure ("'" <> Text.pack word <> "'")
  shrink (Tsvector base) =
    if Text.null base
      then []
      else [Tsvector ""]

instance IsStandardType Tsvector where
  typeName = Tagged "tsvector"
  baseOid = Tagged 3614
  arrayOid = Tagged 3643
  binaryEncoder (Tsvector base) =
    -- TODO: Implement full PostgreSQL binary protocol for tsvector
    -- For now, encode as UTF-8 text (simplified approach)
    Write.textUtf8 base
  binaryDecoder = do
    bytes <- PtrPeeker.remainderAsByteString
    case Text.Encoding.decodeUtf8' bytes of
      Left e ->
        pure
          ( Left
              ( DecodingError
                  { location = ["tsvector"],
                    reason =
                      ParsingDecodingErrorReason
                        (fromString (show e))
                        bytes
                  }
              )
          )
      Right base -> pure (Right (Tsvector base))
  textualEncoder (Tsvector base) = TextBuilder.text base
  textualDecoder = Tsvector <$> Attoparsec.takeText

-- | Conversion from Haskell 'Data.Text.Text'.
-- Fails if the text contains null characters (not supported by PostgreSQL).
instance IsSome Text.Text Tsvector where
  to (Tsvector t) = t
  maybeFrom text =
    if Text.elem '\NUL' text
      then Nothing
      else Just (Tsvector text)

-- | Total conversion from Haskell 'Data.Text.Text'.
-- Strips null characters to ensure PostgreSQL compatibility.
instance IsMany Text.Text Tsvector where
  onfrom = Tsvector . Text.replace "\NUL" ""
