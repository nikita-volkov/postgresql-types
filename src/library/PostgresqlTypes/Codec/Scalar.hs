{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints -Wno-deprecations -Wno-missing-signatures #-}

module PostgresqlTypes.Codec.Scalar where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import PostgresqlTypes.Codec.DecodingError
import PostgresqlTypes.Codec.Prelude
import qualified PostgresqlTypes.Primitive as Primitive
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- TODO: Keep this constructor open for extensibility.
-- Or just provide a 'custom' constructor.
-- OTOH, keeping the constructor open also serves for portability:
-- it can be the point of integration with libraries interpreting it.
data Scalar a = Scalar
  { -- | Schema name. If empty, the default schema will be used.
    schemaName :: Text,
    -- | Type name.
    typeName :: Text,
    -- | Statically known OID for the type.
    -- When unspecified, the OID may be determined at runtime by looking up by name.
    baseOid :: Maybe Word32,
    -- | Statically known OID for the array-type with this type as the element.
    -- When unspecified, the OID may be determined at runtime by looking up by name.
    -- It may also mean that there may be no array type containing this type, which is the case in attempts to double-nest arrays.
    arrayOid :: Maybe Word32,
    binaryEncoder :: a -> Write.Write,
    binaryDecoder :: PtrPeeker.Variable (Either DecodingError a),
    -- | Represent in Postgres textual format.
    textualEncoder :: a -> TextBuilder.TextBuilder
  }

instance Invariant Scalar where
  invmap f g Scalar {..} =
    Scalar
      { binaryEncoder = binaryEncoder . g,
        binaryDecoder = fmap (fmap f) binaryDecoder,
        textualEncoder = textualEncoder . g,
        ..
      }

-- | Lift a primitive into Scalar.
primitive :: forall a. (Primitive.IsPrimitive a) => Scalar a
primitive =
  let -- Convert decoding errors between layers (they have identical shapes)
      convertError :: Primitive.DecodingError -> DecodingError
      convertError Primitive.DecodingError {location, reason} =
        DecodingError
          { location,
            reason = case reason of
              Primitive.ParsingDecodingErrorReason details input ->
                ParsingDecodingErrorReason details input
              Primitive.UnexpectedValueDecodingErrorReason expected actual ->
                UnexpectedValueDecodingErrorReason expected actual
          }
      tName = untag (Primitive.typeName @a)
      bOid = untag (Primitive.baseOid @a)
      aOid = untag (Primitive.arrayOid @a)
   in Scalar
        { schemaName = "",
          typeName = tName,
          baseOid = Just bOid,
          arrayOid = Just aOid,
          binaryEncoder = Primitive.binaryEncoder,
          binaryDecoder = fmap (first convertError) (Primitive.binaryDecoder @a),
          textualEncoder = Primitive.textualEncoder
        }

enum :: (Ord a) => Text -> Text -> [(Text, a)] -> Scalar a
enum schema tName variants =
  let byValue = Map.fromList (map (\(l, v) -> (v, l)) variants)

      -- Binary encoder: encode as text (enums are text in PostgreSQL wire protocol)
      encoder val =
        case Map.lookup val byValue of
          Just label -> Write.textUtf8 label
          Nothing -> error ("Invalid enum value for type: " <> show tName)

      -- Binary decoder: decode from text and look up in variants map
      decoder = do
        bytes <- PtrPeeker.remainderAsByteString
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
          Right label ->
            case lookup label variants of
              Just val -> pure (Right val)
              Nothing ->
                pure
                  ( Left
                      ( DecodingError
                          { location = [],
                            reason =
                              UnexpectedValueDecodingErrorReason
                                ("One of: " <> Text.intercalate ", " (map fst variants))
                                label
                          }
                      )
                  )

      -- Textual encoder: just output the label
      textEncoder val =
        case lookup val (map (\(l, v) -> (v, l)) variants) of
          Just label -> TextBuilder.text label
          Nothing -> error ("Invalid enum value for type: " <> show tName)
   in Scalar
        { schemaName = schema,
          typeName = tName,
          baseOid = Nothing,
          arrayOid = Nothing,
          binaryEncoder = encoder,
          binaryDecoder = decoder,
          textualEncoder = textEncoder
        }
