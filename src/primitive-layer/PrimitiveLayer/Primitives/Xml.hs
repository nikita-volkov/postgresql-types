-- | PostgreSQL @xml@ type.
-- Represents XML data in PostgreSQL.
module PrimitiveLayer.Primitives.Xml (Xml (..)) where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @xml@ type wrapper around 'Text'.
-- Represents XML data stored as text in PostgreSQL.
newtype Xml = Xml Text
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Xml)

instance Primitive Xml where
  typeName = Tagged "xml"
  baseOid = Tagged 142
  arrayOid = Tagged 143
  binaryEncoder (Xml text) =
    Write.byteString (Text.Encoding.encodeUtf8 text)
  binaryDecoder = do
    bytes <- PeekyBlinders.remainderAsByteString
    case Text.Encoding.decodeUtf8' bytes of
      Right text -> pure (Right (Xml text))
      Left err ->
        pure
          $ Left
          $ DecodingError
            { location = ["xml"],
              reason =
                ParsingDecodingErrorReason
                  ("UTF-8 decoding error: " <> Text.pack (show err))
                  bytes
            }
  textualEncoder (Xml text) = TextBuilder.text text

-- | Direct conversion from 'Text'.
-- This is always safe since both types represent text data identically.
instance IsSome Text Xml where
  to (Xml text) = text
  maybeFrom = Just . Xml

-- | Direct conversion from Xml to 'Text'.
-- This is always safe since both types represent text data identically.
instance IsSome Xml Text where
  to text = Xml text
  maybeFrom (Xml text) = Just text

-- | Direct conversion from 'Text'.
-- This is a total conversion as it always succeeds.
instance IsMany Text Xml where
  from = Xml

-- | Direct conversion from Xml to 'Text'.
-- This is a total conversion as it always succeeds.
instance IsMany Xml Text where
  from (Xml text) = text

-- | Bidirectional conversion between 'Text' and Xml.
instance Is Text Xml

instance Is Xml Text
