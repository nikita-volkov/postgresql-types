module PrimitiveLayer.Types.Oid (Oid) where

import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @oid@ type. Object identifier.
--
-- Range: @0@ to @4294967295@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-oid.html).
newtype Oid = Oid Word32
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsPrimitive Oid)

instance IsPrimitive Oid where
  typeName = Tagged "oid"
  baseOid = Tagged 26
  arrayOid = Tagged 1028
  binaryEncoder (Oid x) = Write.bWord32 x
  binaryDecoder = PtrPeeker.fixed (Right . Oid <$> PtrPeeker.beUnsignedInt4)
  textualEncoder (Oid x) = TextBuilder.decimal x

-- | Direct conversion from 'Word32'.
-- This is always safe since both types represent 32-bit unsigned integers identically.
instance IsSome Word32 Oid where
  to (Oid w) = w
  maybeFrom = Just . Oid

-- | Direct conversion from PostgreSQL Oid to 'Word32'.
-- This is always safe since both types represent 32-bit unsigned integers identically.
instance IsSome Oid Word32 where
  to w = Oid w
  maybeFrom (Oid w) = Just w

-- | Direct conversion from 'Word32'.
-- This is a total conversion as it always succeeds.
instance IsMany Word32 Oid where
  onfrom = Oid

-- | Direct conversion from PostgreSQL Oid to 'Word32'.
-- This is a total conversion as it always succeeds.
instance IsMany Oid Word32 where
  onfrom (Oid w) = w

-- | Bidirectional conversion between 'Word32' and PostgreSQL Oid.
instance Is Word32 Oid

instance Is Oid Word32
