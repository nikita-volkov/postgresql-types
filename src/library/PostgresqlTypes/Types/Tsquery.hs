module PostgresqlTypes.Types.Tsquery (Tsquery) where

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

-- | PostgreSQL @tsquery@ type. Text search query.
--
-- A tsquery value stores lexemes that are to be searched for, and
-- can combine them using the Boolean operators & (AND), | (OR), and ! (NOT),
-- as well as the phrase search operator <-> (FOLLOWED BY).
--
-- The value is stored internally in PostgreSQL's normalized text format.
--
-- __Note__: The binary encoding/decoding currently uses a simplified implementation.
-- Full PostgreSQL wire protocol support for tsquery is complex and is a work in progress.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-textsearch.html).
newtype Tsquery = Tsquery Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Tsquery)

instance Arbitrary Tsquery where
  arbitrary = do
    -- Generate valid tsquery format: either empty or quoted lexeme(s) with operators
    numTerms <- QuickCheck.chooseInt (0, 3)
    if numTerms == 0
      then pure (Tsquery "")
      else do
        terms <- QuickCheck.vectorOf numTerms genTerm
        pure (Tsquery (Text.intercalate " & " terms))
    where
      genTerm = do
        -- Generate a simple word (lowercase letters)
        len <- QuickCheck.chooseInt (1, 10)
        word <- QuickCheck.vectorOf len (QuickCheck.elements ['a'..'z'])
        pure ("'" <> Text.pack word <> "'")
  shrink (Tsquery base) =
    if Text.null base
      then []
      else [Tsquery ""]

instance IsStandardType Tsquery where
  typeName = Tagged "tsquery"
  baseOid = Tagged 3615
  arrayOid = Tagged 3645
  binaryEncoder (Tsquery base) =
    -- TODO: Implement full PostgreSQL binary protocol for tsquery
    -- For now, encode as UTF-8 text (simplified approach)
    Write.textUtf8 base
  binaryDecoder = do
    bytes <- PtrPeeker.remainderAsByteString
    case Text.Encoding.decodeUtf8' bytes of
      Left e ->
        pure
          ( Left
              ( DecodingError
                  { location = ["tsquery"],
                    reason =
                      ParsingDecodingErrorReason
                        (fromString (show e))
                        bytes
                  }
              )
          )
      Right base -> pure (Right (Tsquery base))
  textualEncoder (Tsquery base) = TextBuilder.text base
  textualDecoder = Tsquery <$> Attoparsec.takeText

-- | Conversion from Haskell 'Data.Text.Text'.
-- Fails if the text contains null characters (not supported by PostgreSQL).
instance IsSome Text.Text Tsquery where
  to (Tsquery t) = t
  maybeFrom text =
    if Text.elem '\NUL' text
      then Nothing
      else Just (Tsquery text)

-- | Total conversion from Haskell 'Data.Text.Text'.
-- Strips null characters to ensure PostgreSQL compatibility.
instance IsMany Text.Text Tsquery where
  onfrom = Tsquery . Text.replace "\NUL" ""
