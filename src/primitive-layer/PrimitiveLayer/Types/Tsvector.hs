module PrimitiveLayer.Types.Tsvector (Tsvector) where

import qualified Data.ByteString as ByteString
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (Text)
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @tsvector@ type. Text search vector containing lexemes and their positions.
--
-- Represents a sorted list of distinct lexemes (words) which are preprocessed
-- for text search. Each lexeme may have position information indicating where
-- it appeared in the original document.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-textsearch.html).
newtype Tsvector = Tsvector Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Tsvector)

instance Arbitrary Tsvector where
  arbitrary =
    Tsvector <$> do
      charList <- QuickCheck.listOf do
        QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
      pure (Text.pack charList)
  shrink (Tsvector base) =
    Tsvector . Text.pack <$> shrink (Text.unpack base)

instance Mapping Tsvector where
  typeName = Tagged "tsvector"
  baseOid = Tagged 3614
  arrayOid = Tagged 3643
  binaryEncoder (Tsvector base) = Write.textUtf8 base
  binaryDecoder = do
    bytes <- PeekyBlinders.remainderAsByteString
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
      Right base -> pure (Right (Tsvector base))
  textualEncoder (Tsvector base) = TextBuilder.text base

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