module PostgresqlTypes.Primitive.Algebra where

import PostgresqlTypes.Primitive.Prelude
import qualified PostgresqlTypes.Primitive.Writes as Writes
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | Evidence that a type maps to a PostgreSQL value.
class IsPrimitive a where
  -- | PostgreSQL type name.
  typeName :: Tagged a Text

  -- | Statically known OID for the base type.
  baseOid :: Tagged a Word32

  -- | Statically known OID for the array type.
  arrayOid :: Tagged a Word32

  -- | Encode the value in PostgreSQL binary format.
  binaryEncoder :: a -> Write.Write

  -- | Decode the value from PostgreSQL binary format.
  binaryDecoder :: PtrPeeker.Variable (Either DecodingError a)

  -- | Represent the value in PostgreSQL textual format.
  textualEncoder :: a -> TextBuilder.TextBuilder

-- | Evidence that a type can be used as an element of a PostgreSQL range type.
class (IsPrimitive a) => IsRangeElement a where
  -- | PostgreSQL range type name.
  rangeTypeName :: Tagged a Text

  -- | Statically known OID for the range type.
  rangeOid :: Tagged a Word32

  -- | Statically known OID for the range array-type.
  rangeArrayOid :: Tagged a Word32

-- | Evidence that a type can be used as an element of a PostgreSQL multirange type.
class (IsRangeElement a) => IsMultirangeElement a where
  -- | PostgreSQL multirange type name.
  multirangeTypeName :: Tagged a Text

  -- | Statically known OID for the multirange type.
  multirangeOid :: Tagged a Word32

  -- | Statically known OID for the multirange array-type.
  multirangeArrayOid :: Tagged a Word32

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
  | InvalidValueDecodingErrorReason
      -- | Details.
      Text
      -- | Value.
      Text
  deriving stock (Show, Eq)
