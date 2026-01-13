module PostgresqlTypes.Codec.Fields where

import PostgresqlTypes.Codec.DecodingError
import PostgresqlTypes.Codec.Dimensionality
import PostgresqlTypes.Codec.Nullability
import PostgresqlTypes.Codec.Prelude
import PostgresqlTypes.Codec.Scalar
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Fields a b = Fields
  { count :: Int32,
    binaryEncoder :: a -> Write.Write,
    binaryDecoder :: PtrPeeker.Variable (Either DecodingError b),
    -- | Represent in Postgres textual format.
    textualEncoder :: a -> [TextBuilder.TextBuilder]
  }

instance Functor (Fields a) where
  fmap f Fields {..} =
    Fields
      { count,
        binaryEncoder,
        binaryDecoder = fmap (fmap f) binaryDecoder,
        textualEncoder
      }

instance Applicative (Fields a) where
  pure x =
    Fields
      { count = 0,
        binaryEncoder = const mempty,
        binaryDecoder = pure (Right x),
        textualEncoder = const []
      }
  (<*>) left right =
    Fields
      { count = left.count + right.count,
        binaryEncoder = \value -> left.binaryEncoder value <> right.binaryEncoder value,
        binaryDecoder = liftA2 (<*>) left.binaryDecoder right.binaryDecoder,
        textualEncoder = \value -> left.textualEncoder value <> right.textualEncoder value
      }

instance Profunctor Fields where
  dimap f g Fields {..} =
    Fields
      { count,
        binaryEncoder = \value -> binaryEncoder (f value),
        binaryDecoder = fmap (fmap g) binaryDecoder,
        textualEncoder = \value -> textualEncoder (f value)
      }

field :: Scalar a -> Dimensionality a b -> Nullability b c -> Fields c c
field scalar dimensionality nullability =
  let baseOid = fromMaybe 705 scalar.baseOid
   in Fields
        { count = 1,
          binaryEncoder =
            nullability.binaryEncoderWrapper
              (dimensionality.binaryEncoder baseOid scalar.binaryEncoder),
          binaryDecoder =
            nullability.binaryDecoderWrapper
              (dimensionality.binaryDecoder baseOid scalar.binaryDecoder),
          textualEncoder =
            pure
              . nullability.textualEncoderWrapper
                (dimensionality.textualEncoder scalar.textualEncoder)
        }

composite :: Text -> Text -> Fields a a -> Scalar a
composite schemaName typeName fields =
  Scalar
    { schemaName,
      typeName,
      baseOid = Nothing,
      arrayOid = Nothing,
      binaryEncoder = \value ->
        Write.lInt32 fields.count <> fields.binaryEncoder value,
      binaryDecoder =
        runExceptT do
          _ <-
            ExceptT do
              PtrPeeker.fixed do
                PtrPeeker.beSignedInt4
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
