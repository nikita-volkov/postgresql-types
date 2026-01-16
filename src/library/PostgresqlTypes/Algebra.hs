module PostgresqlTypes.Algebra where

import qualified Data.Attoparsec.Text as Attoparsec
import PostgresqlTypes.Prelude
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Evidence that a type maps to a PostgreSQL value.
class IsStandardType a where
  -- | PostgreSQL type name.
  typeName :: Tagged a Text

  -- | PostgreSQL type OID, if known at compile time.
  baseOid :: Tagged a (Maybe Word32)

  -- | PostgreSQL array type OID, if known at compile time.
  arrayOid :: Tagged a (Maybe Word32)

  -- | Extract the type parameters from a particular value at runtime.
  --
  -- E.g., in case of the @bit@ type, this can be use to translate the length parameter (the @n@ in @bit(n)@).
  runtimeTypeParams :: a -> [Text]

  -- | Encode the value in PostgreSQL binary format.
  binaryEncoder :: a -> Write.Write

  -- | Decode the value from PostgreSQL binary format.
  binaryDecoder :: PtrPeeker.Variable (Either DecodingError a)

  -- | Represent the value in PostgreSQL textual format.
  textualEncoder :: a -> TextBuilder.TextBuilder

  -- | Decode the value from PostgreSQL textual format.
  textualDecoder :: Attoparsec.Parser a

-- | Evidence that a type can be used as an element of a PostgreSQL range type.
class (IsStandardType a, Ord a) => IsRangeElement a where
  -- | PostgreSQL range type name.
  rangeTypeName :: Tagged a Text

  -- | Statically known OID for the range type.
  rangeBaseOid :: Tagged a (Maybe Word32)

  -- | Statically known OID for the range array-type.
  rangeArrayOid :: Tagged a (Maybe Word32)

-- | Evidence that a type can be used as an element of a PostgreSQL multirange type.
class (IsRangeElement a) => IsMultirangeElement a where
  -- | PostgreSQL multirange type name.
  multirangeTypeName :: Tagged a Text

  -- | Statically known OID for the multirange type.
  multirangeBaseOid :: Tagged a (Maybe Word32)

  -- | Statically known OID for the multirange array-type.
  multirangeArrayOid :: Tagged a (Maybe Word32)

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
  | -- | Unsupported server-side value.
    UnsupportedValueDecodingErrorReason
      -- | Details.
      Text
      -- | Value.
      Text
  deriving stock (Show, Eq)

toTypeSignature :: forall a. (IsStandardType a) => a -> Text
toTypeSignature value =
  let params = runtimeTypeParams value
   in if null params
        then untag (typeName @a)
        else
          TextBuilder.toText
            ( mconcat
                [ TextBuilder.text (untag (typeName @a)),
                  "(",
                  TextBuilder.intercalateMap ", " (TextBuilder.text) params,
                  ")"
                ]
            )
