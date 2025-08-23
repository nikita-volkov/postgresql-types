module PrimitiveLayer.Algebra where

import qualified PeekyBlinders
import PrimitiveLayer.Prelude
import qualified PrimitiveLayer.Writes as Writes
import qualified PtrPoker.Write as Write
import qualified TextBuilder

class Mapping a where
  -- | PostgreSQL type name.
  typeName :: Tagged a Text

  -- | Statically known OID for the base type.
  baseOid :: Tagged a Word32

  -- | Statically known OID for the array type.
  arrayOid :: Tagged a Word32

  -- | Encode the value in PostgreSQL binary format.
  binaryEncoder :: a -> Write.Write

  -- | Decode the value from PostgreSQL binary format.
  binaryDecoder :: PeekyBlinders.Dynamic (Either DecodingError a)

  -- | Represent the value in PostgreSQL textual format.
  textualEncoder :: a -> TextBuilder.TextBuilder

class (Mapping a) => RangeMapping a where
  -- | PostgreSQL range type name.
  rangeTypeName :: Tagged a Text

  -- | Statically known OID for the range type.
  rangeOid :: Tagged a Word32

  -- | Statically known OID for the range array-type.
  rangeArrayOid :: Tagged a Word32

class (RangeMapping a) => MultirangeMapping a where
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
