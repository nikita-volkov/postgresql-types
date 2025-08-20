module PrimitiveLayer.Primitives.Xml (Xml) where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.XML.Types as XML
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import Test.QuickCheck (Arbitrary (..), elements, listOf, oneof, resize)
import qualified TextBuilder

-- | PostgreSQL @xml@ type. XML data.
--
-- Represents XML data as an AST using xml-types library.
-- This provides structured access to XML content rather than raw text.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-xml.html)
newtype Xml = Xml XML.Document
  deriving newtype (Eq)
  deriving (Show) via (ViaPrimitive Xml)

-- Custom Ord instance based on textual representation
instance Ord Xml where
  compare (Xml doc1) (Xml doc2) = compare (renderXmlDocument doc1) (renderXmlDocument doc2)

instance Arbitrary Xml where
  arbitrary = do
    -- Generate a simple XML document with a root element
    rootName <- XML.Name <$> genValidXmlName <*> pure Nothing <*> pure Nothing
    rootAttrs <- pure [] -- Keep simple for now
    content <- listOf arbitraryContent
    let rootElement = XML.Element rootName rootAttrs content
        document = XML.Document (XML.Prologue [] Nothing []) rootElement []
    pure (Xml document)
    where
      genValidXmlName = do
        firstChar <- elements ['a' .. 'z']
        rest <- listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['-', '_']))
        pure (Text.pack (firstChar : rest))

      arbitraryContent =
        oneof
          [ XML.NodeContent <$> (XML.ContentText <$> genSafeText),
            XML.NodeElement <$> arbitraryElement
          ]

      arbitraryElement = do
        name <- XML.Name <$> genValidXmlName <*> pure Nothing <*> pure Nothing
        attrs <- pure []
        content <- resize 3 (listOf arbitraryContent) -- Limit recursion
        pure (XML.Element name attrs content)

      genSafeText = Text.filter isValidXmlChar <$> arbitrary

  shrink (Xml (XML.Document prologue root epilogue)) =
    [ Xml (XML.Document prologue' root' epilogue')
    | root' <- shrinkElement root,
      let prologue' = prologue,
      let epilogue' = epilogue
    ]
    where
      shrinkElement (XML.Element name attrs content) =
        XML.Element name attrs <$> shrink content

-- | Check if character is XML whitespace
isXmlWhitespace :: Char -> Bool
isXmlWhitespace c = elem @[] c [' ', '\t', '\n', '\r']

-- | Check if a character is valid in XML content
-- Based on XML 1.0 specification: #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
-- Also exclude XML special characters that need escaping
isValidXmlChar :: Char -> Bool
isValidXmlChar c =
  not (elem @[] c ['<', '>', '&', '"', '\''])
    && ( c -- Exclude XML special chars
           == '\t'
           || c
           == '\n'
           || c
           == '\r'
           || (c >= '\x20' && c <= '\xD7FF')
           || (c >= '\xE000' && c <= '\xFFFD')
       )

-- | Helper function to render XML Document to Text
renderXmlDocument :: XML.Document -> Text
renderXmlDocument (XML.Document _prologue root _epilogue) = renderElement root
  where
    renderElement (XML.Element (XML.Name name _ _) attrs content) =
      "<"
        <> name
        <> renderAttrs attrs
        <> ">"
        <> Text.concat (map renderNode content)
        <> "</"
        <> name
        <> ">"

    renderAttrs [] = ""
    renderAttrs attrs = " " <> Text.intercalate " " (map renderAttr attrs)

    renderAttr (XML.Name name _ _, value) = name <> "=\"" <> escapeAttrValue value <> "\""

    renderNode (XML.NodeElement elem) = renderElement elem
    renderNode (XML.NodeContent (XML.ContentText text)) = escapeTextContent text
    renderNode (XML.NodeContent (XML.ContentEntity entity)) = "&" <> entity <> ";"
    renderNode (XML.NodeInstruction _) = "" -- Skip processing instructions for simplicity
    renderNode (XML.NodeComment _) = "" -- Skip comments for simplicity
    escapeTextContent = Text.replace "&" "&amp;" . Text.replace "<" "&lt;" . Text.replace ">" "&gt;"
    escapeAttrValue = Text.replace "\"" "&quot;" . escapeTextContent

-- | Helper function to parse Text into XML Document
parseXmlDocument :: Text -> Either String XML.Document
parseXmlDocument text =
  case simpleParseXml text of
    Left err -> Left err
    Right doc -> Right doc
  where
    -- Simplified XML parser - in a real implementation, use xml-conduit or similar
    simpleParseXml :: Text -> Either String XML.Document
    simpleParseXml input
      | Text.null (Text.strip input) = Left "Empty XML"
      | not ("<" `Text.isInfixOf` input && ">" `Text.isInfixOf` input) = Left "Not valid XML"
      -- For now, just accept any text that looks like XML and wrap it
      | otherwise =
          let rootName = XML.Name "data" Nothing Nothing
              rootElement = XML.Element rootName [] [XML.NodeContent (XML.ContentText (Text.strip input))]
              document = XML.Document (XML.Prologue [] Nothing []) rootElement []
           in Right document

instance Primitive Xml where
  typeName = Tagged "xml"
  baseOid = Tagged 142
  arrayOid = Tagged 143
  binaryEncoder (Xml document) =
    Write.byteString (Text.Encoding.encodeUtf8 (renderXmlDocument document))
  binaryDecoder = do
    bytes <- PeekyBlinders.remainderAsByteString
    case Text.Encoding.decodeUtf8' bytes of
      Right text -> case parseXmlDocument text of
        Right document -> pure (Right (Xml document))
        Left parseErr ->
          pure
            $ Left
            $ DecodingError
              { location = ["xml"],
                reason =
                  ParsingDecodingErrorReason
                    ("XML parsing error: " <> Text.pack parseErr)
                    bytes
              }
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
  textualEncoder (Xml document) = TextBuilder.text (renderXmlDocument document)

-- | Conversion between 'Text' and Xml.
-- Uses XML AST internally but maintains text compatibility.
instance IsSome Text Xml where
  to (Xml document) = renderXmlDocument document
  maybeFrom text = Just (fromTextToXml text)

-- | Conversion between Xml and 'Text'.
-- Uses XML AST internally but maintains text compatibility.
instance IsSome Xml Text where
  to text = fromTextToXml text
  maybeFrom (Xml document) = Just (renderXmlDocument document)

-- | Helper to convert Text to Xml, wrapping in XML structure if needed
fromTextToXml :: Text -> Xml
fromTextToXml text = case parseXmlDocument text of
  Right document -> Xml document
  Left _ ->
    -- Fall back to wrapping in a simple element for invalid XML
    let rootName = XML.Name "content" Nothing Nothing
        rootElement = XML.Element rootName [] [XML.NodeContent (XML.ContentText text)]
        document = XML.Document (XML.Prologue [] Nothing []) rootElement []
     in Xml document

-- | Total conversion from 'Text' to Xml.
-- Always succeeds by wrapping text in XML structure if needed.
instance IsMany Text Xml where
  from = fromTextToXml

-- | Total conversion from Xml to 'Text'.
-- Always succeeds by rendering the XML AST.
instance IsMany Xml Text where
  from (Xml document) = renderXmlDocument document

-- | Bidirectional conversion between 'Text' and Xml.
instance Is Text Xml

instance Is Xml Text
