module PrimitiveLayer.Algebra where

import qualified PeekyBlinders
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

class Primitive a where
  -- | PostgreSQL type name.
  typeName :: Tagged a Text

  -- | Statically known OID for the base type.
  baseOid :: Tagged a Int32

  -- | Statically known OID for the array type.
  arrayOid :: Tagged a Int32

  -- | Encode the value in PostgreSQL binary format.
  binaryEncoder :: a -> Write.Write

  -- | Decode the value from PostgreSQL binary format.
  binaryDecoder :: PeekyBlinders.Dynamic (Either DecodingError a)

  -- | Represent the value in PostgreSQL textual format.
  textualEncoder :: a -> TextBuilder.TextBuilder

newtype ViaPrimitive a = ViaPrimitive a
  deriving newtype (Eq, Ord, Arbitrary, Primitive)

instance (Primitive a) => Show (ViaPrimitive a) where
  showsPrec d (ViaPrimitive a) = showsPrec d (textualEncoder a)

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
