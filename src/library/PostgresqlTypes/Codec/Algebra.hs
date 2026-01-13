{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-imports -Wno-name-shadowing -Wno-incomplete-patterns -Wno-unused-matches -Wno-missing-methods -Wno-unused-record-wildcards -Wno-redundant-constraints #-}

module PostgresqlTypes.Codec.Algebra where

import qualified Data.Vector as Vector
import qualified PostgresqlTypes.Codec.Algebra.Writes as Writes
import PostgresqlTypes.Codec.Prelude
import qualified PostgresqlTypes.Primitive as Primitive
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

data DecodingError = DecodingError
  { location :: [Text],
    reason :: DecodingErrorReason
  }
  deriving stock (Show, Eq)

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
  deriving stock (Show, Eq)

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

data Dimensionality scalar vec = Dimensionality
  { binaryEncoder ::
      Word32 ->
      (scalar -> Write.Write) ->
      (vec -> Write.Write),
    binaryDecoder ::
      Word32 ->
      PtrPeeker.Variable (Either DecodingError scalar) ->
      PtrPeeker.Variable (Either DecodingError vec),
    textualEncoder ::
      (scalar -> TextBuilder.TextBuilder) ->
      (vec -> TextBuilder.TextBuilder)
  }

d0 :: Dimensionality a a
d0 =
  Dimensionality
    { binaryEncoder = const id,
      binaryDecoder = const id,
      textualEncoder = id
    }

d1 :: Nullability a b -> Dimension b c -> Dimensionality a c
d1 elementNullability (Dimension construct1 destruct1 count1) =
  Dimensionality
    { binaryEncoder =
        \baseOid scalarEncoder vec ->
          let head =
                Writes.arrayHeader 1 elementNullability.nullable baseOid [count1 vec]
              elementEncoder =
                elementNullability.binaryEncoderWrapper scalarEncoder
              body =
                destruct1
                  ( \accumulator element ->
                      accumulator <> elementEncoder element
                  )
                  head
                  vec
           in head <> body,
      binaryDecoder =
        \expectedBaseOid scalarDecoder -> runExceptT do
          (dimensionCount, hasNulls, baseOid) <- lift do
            PtrPeeker.fixed do
              dimensionCount <- PtrPeeker.beSignedInt4
              hasNulls <- do
                int <- PtrPeeker.beSignedInt4
                pure (int == 1)
              baseOid <- PtrPeeker.beUnsignedInt4
              pure (dimensionCount, hasNulls, baseOid)

          -- Interrupt early if baseOid does not match the expected one.
          when (baseOid /= expectedBaseOid) do
            throwError
              DecodingError
                { location = ["base-oid"],
                  reason =
                    UnexpectedValueDecodingErrorReason
                      (TextBuilder.toText (TextBuilder.decimal expectedBaseOid))
                      (TextBuilder.toText (TextBuilder.decimal baseOid))
                }

          -- Interrupt early if the amount of dimensions is wrong.
          when (dimensionCount /= 1) do
            throwError
              DecodingError
                { location = ["dimension-count"],
                  reason =
                    UnexpectedValueDecodingErrorReason
                      (TextBuilder.toText (TextBuilder.decimal 1))
                      (TextBuilder.toText (TextBuilder.decimal dimensionCount))
                }

          -- Interrupt early if the array contains nulls and they are not expected.
          when (hasNulls && not elementNullability.nullable) do
            throwError
              DecodingError
                { location = ["contains-nulls"],
                  reason = UnexpectedValueDecodingErrorReason "false" "true"
                }

          (size1, lowerBound1) <- lift do
            PtrPeeker.fixed do
              liftA2 (,) PtrPeeker.beSignedInt4 PtrPeeker.beSignedInt4

          -- Interrupt early if the lower-bound value is not supported.
          when (lowerBound1 /= 1) do
            throwError
              DecodingError
                { location = ["lower-bound"],
                  reason =
                    UnexpectedValueDecodingErrorReason
                      (TextBuilder.toText (TextBuilder.decimal 1))
                      (TextBuilder.toText (TextBuilder.decimal lowerBound1))
                }

          construct1 (fromIntegral size1) do
            ExceptT do
              elementNullability.binaryDecoderWrapper scalarDecoder,
      textualEncoder = \scalarEncoder vec ->
        error "TODO"
    }

d2 :: Nullability a b -> Dimension b c -> Dimension c d -> Dimensionality a d
d2 = error "TODO"

data Dimension scalar vec = Dimension
  { -- | For constructing the dimension representation during decoding.
    construct :: forall m. (Monad m) => Int -> m scalar -> m vec,
    -- | For destructuring the dimension representation during encoding.
    destruct :: forall a. (a -> scalar -> a) -> (a -> vec -> a),
    count :: vec -> Int32
  }

vector :: Dimension a (Vector a)
vector =
  Dimension
    { construct = Vector.replicateM,
      destruct = Vector.foldl',
      count = fromIntegral . Vector.length
    }

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

composite :: forall a. (IsComposite a) => Scalar a
composite =
  let fields = compositeCodec @a
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

enum :: (IsEnum a) => Scalar a
enum =
  error "TODO"

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

-- | Parameterized query parameters encoder.
data Params a = Params
  { count :: Int32,
    binaryEncoder :: [a -> Maybe Write.Write],
    textualEncoder :: [a -> TextBuilder.TextBuilder]
  }

instance Contravariant Params

instance Semigroup (Params a)

instance Monoid (Params a)

param :: Scalar a -> Dimensionality a b -> Nullability b c -> Params c
param scalar dimensionality nullability = error "TODO"

-- | Result set columns decoder.
data Columns a = Columns
  { count :: Int32,
    binaryDecoder :: [Maybe ByteString] -> Either DecodingError a
  }

instance Functor Columns

instance Applicative Columns

column :: Scalar a -> Dimensionality a b -> Nullability b c -> Columns c
column = error "TODO"

-- | Result decoder.
data Result a
  = SingleRow (Columns a)
  | forall row. Multirow (Columns row) (Vector row -> a)
  | RowsAffected (Int -> a)

single :: Columns row -> Result row
single = error "TODO"

multirow :: Columns row -> Result (Vector row)
multirow = error "TODO"

-- | Composite type mapping.
class IsComposite a where
  compositeSchema :: Tagged a Text
  compositeName :: Tagged a Text
  compositeCodec :: Fields a a

-- | Enumeration type mapping.
class IsEnum a where
  enumSchema :: Tagged a Text
  enumName :: Tagged a Text

  -- | List of Postgres enumeration label to Haskell value associations.
  enumVariants :: [(Text, a)]

class IsStatement a where
  type ResultOf a
  statementSql :: Tagged a Text
  statementParams :: Params a
  statementResult :: Tagged a (Result (ResultOf a))
