module PrimitiveLayer.Primitives.Text (Text (..)) where

import qualified Data.ByteString as ByteString
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (Text)
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

newtype Text = Text Text.Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Text)

instance Arbitrary Text where
  arbitrary =
    Text <$> do
      charList <- QuickCheck.listOf do
        QuickCheck.suchThat arbitrary (\char -> char /= '\NUL')
      pure (Text.pack charList)
  shrink (Text base) =
    Text . Text.pack <$> shrink (Text.unpack base)

instance Primitive Text where
  typeName = Tagged "text"
  baseOid = Tagged 25
  arrayOid = Tagged 1009
  binaryEncoder (Text base) = Write.textUtf8 base
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
      Right base -> pure (Right (Text base))
  textualEncoder (Text base) = TextBuilder.text base
