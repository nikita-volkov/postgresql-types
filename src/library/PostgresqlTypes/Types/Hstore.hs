module PostgresqlTypes.Types.Hstore (Hstore) where

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude hiding (hGetContents)
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified Test.QuickCheck as QuickCheck
import qualified TextBuilder

-- | PostgreSQL @hstore@ type. Key-value store where keys are text and values are optional text.
--
-- Hstore is a key-value store where both keys and values are text strings.
-- Values can be NULL (represented as Nothing in Haskell).
-- Keys must be unique and cannot be NULL.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/hstore.html).
newtype Hstore = Hstore (Map.Map Text (Maybe Text))
  deriving newtype (Eq, Ord)
  deriving (Show) via (ViaIsScalar Hstore)

instance Arbitrary Hstore where
  arbitrary = do
    -- Generate a list of key-value pairs
    pairs <- QuickCheck.listOf do
      key <- Text.pack <$> QuickCheck.listOf1 (QuickCheck.suchThat arbitrary (\c -> c /= '\NUL' && c /= '=' && c /= '>' && c /= '"' && c /= '\\' && c /= ' ' && c /= ',' && c /= '\n' && c /= '\r' && c /= '\t'))
      value <-
        QuickCheck.oneof
          [ pure Nothing,
            Just . Text.pack <$> QuickCheck.listOf (QuickCheck.suchThat arbitrary (\c -> c /= '\NUL'))
          ]
      pure (key, value)
    pure (Hstore (Map.fromList pairs))
  shrink (Hstore base) =
    Hstore . Map.fromList <$> shrink (Map.toList base)

instance IsScalar Hstore where
  typeName = Tagged "hstore"
  baseOid = Tagged Nothing
  arrayOid = Tagged Nothing
  typeParams = Tagged []
  binaryEncoder (Hstore m) = do
    -- Binary format:
    -- 4 bytes: number of key-value pairs (int32)
    -- For each pair:
    --   4 bytes: key length (int32)
    --   N bytes: key data (UTF-8)
    --   4 bytes: value length (int32), -1 for NULL
    --   M bytes: value data (UTF-8) if not NULL
    Write.bInt32 (fromIntegral (Map.size m))
      <> foldMap encodePair (Map.toList m)
    where
      encodePair (key, maybeValue) =
        let keyBytes = Text.Encoding.encodeUtf8 key
            keyLen = fromIntegral (ByteString.length keyBytes)
         in Write.bInt32 keyLen
              <> Write.byteString keyBytes
              <> case maybeValue of
                Nothing -> Write.bInt32 (-1)
                Just value ->
                  let valueBytes = Text.Encoding.encodeUtf8 value
                      valueLen = fromIntegral (ByteString.length valueBytes)
                   in Write.bInt32 valueLen <> Write.byteString valueBytes

  binaryDecoder = runExceptT do
    pairCount <- lift $ PtrPeeker.fixed PtrPeeker.beSignedInt4
    pairs <- replicateM (fromIntegral pairCount) decodePair
    pure (Hstore (Map.fromList pairs))
    where
      decodePair = do
        keyLen <- lift $ PtrPeeker.fixed PtrPeeker.beSignedInt4
        when (keyLen < 0) do
          throwError (DecodingError ["key-length"] (UnsupportedValueDecodingErrorReason "Key length must be non-negative" (TextBuilder.toText (TextBuilder.decimal keyLen))))
        keyBytes <- lift $ PtrPeeker.forceSize (fromIntegral keyLen) PtrPeeker.remainderAsByteString
        case Text.Encoding.decodeUtf8' keyBytes of
          Left e ->
            throwError
              ( DecodingError
                  { location = ["hstore", "key"],
                    reason = ParsingDecodingErrorReason (fromString (show e)) keyBytes
                  }
              )
          Right key -> do
            valueLen <- lift $ PtrPeeker.fixed PtrPeeker.beSignedInt4
            if valueLen == -1
              then pure (key, Nothing)
              else do
                when (valueLen < 0) do
                  throwError (DecodingError ["value-length"] (UnsupportedValueDecodingErrorReason "Value length must be non-negative or -1 for NULL" (TextBuilder.toText (TextBuilder.decimal valueLen))))
                valueBytes <- lift $ PtrPeeker.forceSize (fromIntegral valueLen) PtrPeeker.remainderAsByteString
                case Text.Encoding.decodeUtf8' valueBytes of
                  Left e ->
                    throwError
                      ( DecodingError
                          { location = ["hstore", "value"],
                            reason = ParsingDecodingErrorReason (fromString (show e)) valueBytes
                          }
                      )
                  Right value -> pure (key, Just value)

  textualEncoder (Hstore m) =
    if Map.null m
      then mempty
      else mconcat $ intersperse (TextBuilder.text ", ") $ map encodePair (Map.toList m)
    where
      encodePair (key, maybeValue) =
        TextBuilder.char '"'
          <> TextBuilder.text (escapeText key)
          <> TextBuilder.text "\"=>"
          <> case maybeValue of
            Nothing -> TextBuilder.text "NULL"
            Just value ->
              TextBuilder.char '"'
                <> TextBuilder.text (escapeText value)
                <> TextBuilder.char '"'
      escapeText = Text.concatMap escapeChar
      escapeChar c = case c of
        '\\' -> "\\\\"
        '"' -> "\\\""
        _ -> Text.singleton c

  textualDecoder = do
    pairs <- Attoparsec.sepBy pairParser (Attoparsec.string ", " <|> Attoparsec.string ",")
    pure (Hstore (Map.fromList pairs))
    where
      pairParser = do
        key <- quotedString
        _ <- Attoparsec.string "=>"
        value <- nullValue <|> (Just <$> quotedString)
        pure (key, value)
      quotedString = do
        _ <- Attoparsec.char '"'
        chars <- many (escapedChar <|> normalChar)
        _ <- Attoparsec.char '"'
        pure (Text.pack chars)
      escapedChar = do
        _ <- Attoparsec.char '\\'
        Attoparsec.anyChar
      normalChar = Attoparsec.satisfy (\c -> c /= '"' && c /= '\\')
      nullValue = do
        _ <- Attoparsec.string "NULL"
        pure Nothing

-- | Conversion from Haskell 'Map.Map Text (Maybe Text)'.
-- Fails if any key or value contains null characters (not supported by PostgreSQL).
instance IsSome (Map.Map Text (Maybe Text)) Hstore where
  to (Hstore m) = m
  maybeFrom m =
    if any hasNul (Map.keys m) || any (maybe False hasNul) (Map.elems m)
      then Nothing
      else Just (Hstore m)
    where
      hasNul = Text.elem '\NUL'

-- | Total conversion from Haskell 'Map.Map Text (Maybe Text)'.
-- Strips null characters to ensure PostgreSQL compatibility.
instance IsMany (Map.Map Text (Maybe Text)) Hstore where
  onfrom m =
    Hstore
      ( Map.fromList
          [ (stripNul k, fmap stripNul v)
          | (k, v) <- Map.toList m
          ]
      )
    where
      stripNul = Text.replace "\NUL" ""
