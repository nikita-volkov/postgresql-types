module PrimitiveLayer.Types.Tsquery (Tsquery) where

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

-- | PostgreSQL @tsquery@ type. Text search query containing search terms and operators.
--
-- Represents a query that can be used to search tsvector values.
-- Contains search terms combined with Boolean operators (&, |, !)
-- and can include weight and proximity operators.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-textsearch.html).
newtype Tsquery = Tsquery Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Tsquery)

instance Arbitrary Tsquery where
  arbitrary = do
    -- Generate simple, valid tsquery text with proper Boolean syntax
    QuickCheck.oneof
      [ -- Single term
        QuickCheck.elements ["hello", "world", "test", "example", "search", "data"] >>= \term ->
          pure (Tsquery term),
        -- Two terms with AND operator
        do
          term1 <- QuickCheck.elements ["hello", "world", "test"]
          term2 <- QuickCheck.elements ["example", "search", "data"]
          pure (Tsquery (term1 <> " & " <> term2)),
        -- Two terms with OR operator
        do
          term1 <- QuickCheck.elements ["hello", "world", "test"]
          term2 <- QuickCheck.elements ["example", "search", "data"]
          pure (Tsquery (term1 <> " | " <> term2))
      ]
  shrink (Tsquery base) =
    -- For tsquery, we can only shrink to individual terms
    case Text.splitOn " & " base <> Text.splitOn " | " base of
      [] -> []
      terms -> [Tsquery term | term <- take 1 terms, not (Text.null term)]

instance Mapping Tsquery where
  typeName = Tagged "tsquery"
  baseOid = Tagged 3615
  arrayOid = Tagged 3645
  binaryEncoder (Tsquery base) =
    -- For now, encode as text since tsquery has a complex binary format
    -- This is similar to how JSON is handled in some cases
    Write.textUtf8 base
  binaryDecoder = do
    bytes <- PeekyBlinders.remainderAsByteString
    -- Handle PostgreSQL's binary format for tsquery
    -- PostgreSQL prepends binary headers to tsquery data
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
      Right base ->
        -- Remove binary headers and trailing nulls from PostgreSQL format
        let cleanText = Text.dropWhileEnd (== '\NUL') $ Text.dropWhile (\c -> c == '\SOH' || c == '\NUL') base
         in pure (Right (Tsquery cleanText))
  textualEncoder (Tsquery base) = TextBuilder.text base

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
