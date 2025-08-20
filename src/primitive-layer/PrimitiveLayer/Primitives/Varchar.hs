module PrimitiveLayer.Primitives.Varchar (Varchar) where

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

-- | @varchar@. Variable-length character string with limit.
--
-- PostgreSQL @varchar@ type wrapper around Haskell 'Data.Text.Text'.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-character.html)
newtype Varchar = Varchar Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Varchar)

instance Arbitrary Varchar where
  arbitrary =
    Varchar <$> do
      charList <- QuickCheck.listOf do
        QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
      pure (Text.pack charList)
  shrink (Varchar base) =
    Varchar . Text.pack <$> shrink (Text.unpack base)

instance Primitive Varchar where
  typeName = Tagged "varchar"
  baseOid = Tagged 1043
  arrayOid = Tagged 1015
  binaryEncoder (Varchar base) = Write.textUtf8 base
  binaryDecoder = do
    bytes <- PeekyBlinders.remainderAsByteString
    pure case Text.Encoding.decodeUtf8' bytes of
      Left e ->
        Left
          ( DecodingError
              { location = [],
                reason =
                  ParsingDecodingErrorReason
                    (fromString (show e))
                    bytes
              }
          )
      Right base ->
        Right (Varchar base)
  textualEncoder (Varchar base) = TextBuilder.text base

instance IsSome Text.Text Varchar where
  to = coerce
  maybeFrom text =
    if Text.elem '\NUL' text
      then Nothing
      else Just (Varchar text)

instance IsMany Text.Text Varchar where
  from = Varchar . Text.replace "\NUL" ""
