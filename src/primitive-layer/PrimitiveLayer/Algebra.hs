module PrimitiveLayer.Algebra where

import qualified PeekyBlinders
import PrimitiveLayer.Prelude
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

data Mapping a = Mapping
  { schemaName :: Maybe Text,
    typeName :: Text,
    -- | Statically known OID for the type.
    -- When unspecified, the OID may be determined at runtime by looking up by name.
    baseOid :: Maybe Int32,
    -- | Statically known OID for the array-type with this type as the element.
    -- When unspecified, the OID may be determined at runtime by looking up by name.
    -- It may also mean that there may be no array type containing this type, which is the case in attempts to double-nest arrays.
    arrayOid :: Maybe Int32,
    binaryEncoder :: a -> Write.Write,
    binaryDecoder :: PeekyBlinders.Dynamic (Either DecodingError a),
    -- | Represent in Postgres textual format.
    textualEncoder :: a -> TextBuilder.TextBuilder
  }
