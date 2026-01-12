module PostgresqlTypes.Primitive.Types.Bytea (Bytea) where

import qualified Data.ByteString as ByteString
import PostgresqlTypes.Primitive.Algebra
import PostgresqlTypes.Primitive.Prelude
import PostgresqlTypes.Primitive.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @bytea@ type. Binary data ("byte array").
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-binary.html).
newtype Bytea = Bytea ByteString
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsPrimitive Bytea)

instance IsPrimitive Bytea where
  typeName = Tagged "bytea"
  baseOid = Tagged 17
  arrayOid = Tagged 1001
  binaryEncoder (Bytea bs) =
    Write.byteString bs
  binaryDecoder =
    Right . Bytea <$> PtrPeeker.remainderAsByteString
  textualEncoder (Bytea bs) =
    "\\x" <> foldMap TextBuilder.hexadecimal (ByteString.unpack bs)

-- | Direct conversion from 'ByteString'.
-- This is always safe since both types represent binary data identically.
instance IsSome ByteString Bytea where
  to (Bytea bs) = bs
  maybeFrom = Just . Bytea

-- | Direct conversion from PostgreSQL Bytea to 'ByteString'.
-- This is always safe since both types represent binary data identically.
instance IsSome Bytea ByteString where
  to bs = Bytea bs
  maybeFrom (Bytea bs) = Just bs

-- | Direct conversion from 'ByteString'.
-- This is a total conversion as it always succeeds.
instance IsMany ByteString Bytea where
  onfrom = Bytea

-- | Direct conversion from PostgreSQL Bytea to 'ByteString'.
-- This is a total conversion as it always succeeds.
instance IsMany Bytea ByteString where
  onfrom (Bytea bs) = bs

-- | Bidirectional conversion between 'ByteString' and PostgreSQL Bytea.
instance Is ByteString Bytea

instance Is Bytea ByteString
