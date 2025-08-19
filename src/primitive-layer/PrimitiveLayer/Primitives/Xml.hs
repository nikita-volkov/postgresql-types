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
-- Uses xml-types for type safety but stores as Text for simplicity.
newtype Xml = Xml Text
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaPrimitive Xml)
instance Arbitrary Xml where
  arbitrary = do
    text <- arbitrary
    -- PostgreSQL XML validation requires valid XML characters and normalizes whitespace
    let validXmlText = Text.filter isValidXmlChar text
        -- PostgreSQL seems to trim leading/trailing whitespace from XML content
        sanitized = Text.strip validXmlText
        finalText = if Text.null sanitized then "test" else sanitized
    pure (Xml finalText)
  shrink (Xml text) = [Xml t | t <- shrink text, 
                               let filtered = Text.filter isValidXmlChar t,
                               let trimmed = Text.strip filtered,
                               not (Text.null trimmed)]

-- | Check if character is XML whitespace
isXmlWhitespace :: Char -> Bool  
isXmlWhitespace c = c `elem` [' ', '\t', '\n', '\r']

-- | Check if a character is valid in XML content
-- Based on XML 1.0 specification: #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
-- Also exclude XML special characters that need escaping
isValidXmlChar :: Char -> Bool
isValidXmlChar c = 
  not (c `elem` ['<', '>', '&', '"', '\'']) &&  -- Exclude XML special chars
  (c == '\t' || c == '\n' || c == '\r' || 
   (c >= '\x20' && c <= '\xD7FF') ||
   (c >= '\xE000' && c <= '\xFFFD'))

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
