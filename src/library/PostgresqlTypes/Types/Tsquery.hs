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
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-textsearch.html).
newtype Tsquery = Tsquery Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsStandardType Tsquery)

instance Arbitrary Tsquery where
  arbitrary =
    Tsquery <$> do
      charList <- QuickCheck.listOf do
        QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
      pure (Text.pack charList)
  shrink (Tsquery base) =
    Tsquery . Text.pack <$> shrink (Text.unpack base)

instance IsStandardType Tsquery where
  typeName = Tagged "tsquery"
  baseOid = Tagged 3615
  arrayOid = Tagged 3645
  binaryEncoder (Tsquery base) = Write.textUtf8 base
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
