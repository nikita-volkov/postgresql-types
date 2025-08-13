module PostgresqlTypes.Algebra where

import qualified PeekyBlinders
import PostgresqlTypes.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- * Class layer

class PostgresqlType a where
  mapping :: Mapping a

newtype ViaPostgresqlType a = ViaPostgresqlType a
  deriving newtype (Eq, Ord, Arbitrary, PostgresqlType)

instance (PostgresqlType a) => Show (ViaPostgresqlType a) where
  showsPrec d (ViaPostgresqlType a) = showsPrec d (mapping.textualEncoder a)

-- * Definition layer

data DecodingError = DecodingError
  { location :: [Text],
    reason :: DecodingErrorReason
  }

data DecodingErrorReason
  = ParsingDecodingErrorReason
      -- | Details.
      Text
      -- | Input.
      ByteString
  | UnexpectedValueDecodingErrorReason
      -- | Expected.
      Text
      -- | Actual.
      Text

data Mapping a = Mapping
  { schemaName :: Maybe Text,
    typeName :: Text,
    -- | Statically known OID for the type.
    -- When unspecified, the OID will be determined at runtime by looking up by name.
    baseOid :: Maybe Int32,
    -- | Statically known OID for the array-type.
    -- When unspecified, the OID will be determined at runtime by looking up by name.
    arrayOid :: Maybe Int32,
    binaryEncoder :: a -> Write.Write,
    binaryDecoder :: PeekyBlinders.Dynamic (Either DecodingError a),
    -- | Represent in Postgres textual format.
    textualEncoder :: a -> TextBuilder.TextBuilder
  }

mappingProperties ::
  (Arbitrary a) =>
  Mapping a ->
  [(Text, Property)]
mappingProperties _mapping =
  error "TODO"

composite ::
  -- | Schema name.
  Maybe Text ->
  -- | Type name.
  Text ->
  -- | Fields of the composite type.
  Fields a a ->
  Mapping a
composite schemaName typeName fields =
  Mapping
    { schemaName,
      typeName,
      baseOid = Nothing,
      arrayOid = Nothing,
      binaryEncoder = \value ->
        Write.lWord32 fields.count
          <> fields.binaryEncoder value,
      binaryDecoder =
        runExceptT do
          _ <-
            ExceptT do
              PeekyBlinders.statically do
                PeekyBlinders.beUnsignedInt4
                  <&> \count ->
                    if count == fields.count
                      then Right ()
                      else
                        Left
                          DecodingError
                            { location = ["field-count"],
                              reason =
                                UnexpectedValueDecodingErrorReason
                                  (TextBuilder.toText (TextBuilder.decimal fields.count))
                                  (TextBuilder.toText (TextBuilder.decimal count))
                            }
          ExceptT fields.binaryDecoder,
      textualEncoder = \_value -> error "TODO"
    }

data Fields a b = Fields
  { count :: Word32,
    binaryEncoder :: a -> Write.Write,
    binaryDecoder :: PeekyBlinders.Dynamic (Either DecodingError b),
    -- | Represent in Postgres textual format.
    textualEncoder :: a -> TextBuilder.TextBuilder
  }

field :: Mapping a -> Fields a a
field mapping =
  Fields
    { count = 1,
      binaryEncoder = \value ->
        let write = mapping.binaryEncoder value
         in Write.bInt32 (fromIntegral (Write.writeSize write))
              <> write,
      binaryDecoder = mapping.binaryDecoder,
      textualEncoder = mapping.textualEncoder
    }

nullableField :: Mapping a -> Fields (Maybe a) (Maybe a)
nullableField mapping =
  Fields
    { count = 1,
      binaryEncoder =
        \case
          Nothing -> Write.bInt32 (-1)
          Just value ->
            let write = mapping.binaryEncoder value
             in Write.bInt32 (fromIntegral (Write.writeSize write))
                  <> write,
      binaryDecoder =
        error "TODO",
      textualEncoder = \case
        Nothing -> "NULL"
        Just value -> mapping.textualEncoder value
    }
