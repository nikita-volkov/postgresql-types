module PostgresqlTypes.Codec.Nullability where

import PostgresqlTypes.Codec.DecodingError
import PostgresqlTypes.Codec.Prelude
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data Nullability a b = Nullability
  { nullable :: Bool,
    binaryDecoderWrapper ::
      PtrPeeker.Variable (Either DecodingError a) ->
      PtrPeeker.Variable (Either DecodingError b),
    binaryEncoderWrapper ::
      (a -> Write.Write) ->
      (b -> Write.Write),
    textualEncoderWrapper ::
      (a -> TextBuilder.TextBuilder) ->
      (b -> TextBuilder.TextBuilder)
  }

nullable :: Nullability a (Maybe a)
nullable =
  Nullability
    { nullable = True,
      binaryDecoderWrapper = \contentDecoder -> do
        contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
        case contentSize of
          -1 ->
            pure (Right Nothing)
          _ ->
            fmap (fmap Just) contentDecoder,
      binaryEncoderWrapper = \contentEncoder -> \case
        Just a ->
          let content = contentEncoder a
              contentSize = Write.bInt32 (fromIntegral (Write.writeSize content))
           in contentSize <> content
        Nothing -> Write.bInt32 (-1),
      textualEncoderWrapper = \contentEncoder -> \case
        Just a -> contentEncoder a
        Nothing -> "NULL"
    }

nonNullable :: Nullability a a
nonNullable =
  Nullability
    { nullable = False,
      binaryDecoderWrapper = \contentDecoder -> do
        contentSize <- PtrPeeker.fixed PtrPeeker.beSignedInt4
        case contentSize of
          -1 ->
            pure
              ( Left
                  DecodingError
                    { location = ["content-nullability"],
                      reason = UnexpectedValueDecodingErrorReason "false" "true"
                    }
              )
          _ ->
            contentDecoder,
      binaryEncoderWrapper = \contentEncoder a ->
        let content = contentEncoder a
            contentSize = Write.bInt32 (fromIntegral (Write.writeSize content))
         in contentSize <> content,
      textualEncoderWrapper = id
    }
