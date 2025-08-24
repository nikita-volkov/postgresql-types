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
  arbitrary =
    Tsquery <$> do
      charList <- QuickCheck.listOf do
        QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
      pure (Text.pack charList)
  shrink (Tsquery base) =
    Tsquery . Text.pack <$> shrink (Text.unpack base)

instance Mapping Tsquery where
  typeName = Tagged "tsquery"
  baseOid = Tagged 3615
  arrayOid = Tagged 3645
  binaryEncoder (Tsquery base) = Write.textUtf8 base
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
      Right base -> pure (Right (Tsquery base))
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