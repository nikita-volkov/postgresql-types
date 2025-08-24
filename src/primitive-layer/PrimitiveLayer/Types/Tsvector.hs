module PrimitiveLayer.Types.Tsvector (Tsvector) where

import qualified Data.ByteString as ByteString
import Data.List (nub)
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
  arbitrary = do
    -- Generate simple, valid tsvector text with unique words
    words <- QuickCheck.listOf1 $ QuickCheck.elements ["hello", "world", "test", "example", "search", "data"]
    -- tsvector automatically deduplicates, so we should too
    let uniqueWords = nub words
    pure (Tsvector (Text.unwords uniqueWords))
  shrink (Tsvector base) =
    case Text.words base of
      [] -> []
      [_] -> []
      ws -> [Tsvector (Text.unwords (take n ws)) | n <- [1..length ws - 1]]

instance Mapping Tsvector where
  typeName = Tagged "tsvector"
  baseOid = Tagged 3614
  arrayOid = Tagged 3643
  binaryEncoder (Tsvector base) = 
    -- For now, encode as text since tsvector has a complex binary format
    -- This is similar to how JSON is handled in some cases
    Write.textUtf8 base
  binaryDecoder = do
    bytes <- PeekyBlinders.remainderAsByteString
    -- Handle PostgreSQL's binary format for tsvector
    -- PostgreSQL prepends binary headers to tsvector data
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
      Right base -> 
        -- Remove binary headers and trailing nulls from PostgreSQL format
        let cleanText = Text.dropWhileEnd (== '\NUL') $ Text.dropWhile (\c -> c == '\SOH' || c == '\NUL') base
        in pure (Right (Tsvector cleanText))
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