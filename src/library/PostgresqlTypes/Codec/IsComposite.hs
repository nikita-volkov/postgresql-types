module PostgresqlTypes.Codec.IsComposite where

import PostgresqlTypes.Codec.DecodingError
import PostgresqlTypes.Codec.Fields
import PostgresqlTypes.Codec.Prelude
import PostgresqlTypes.Codec.Scalar
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Composite type mapping.
class IsComposite a where
  compositeSchema :: Tagged a Text
  compositeName :: Tagged a Text
  compositeFields :: Fields a a

composite :: forall a. (IsComposite a) => Scalar a
composite =
  let fields = compositeFields @a
   in Scalar
        { schemaName = untag (compositeSchema @a),
          typeName = untag (compositeName @a),
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
