module PrimitiveLayer.Primitives.Oid (Oid (..)) where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | @oid@. Object identifier. Range: 0 to 4294967295.
--
-- <https://www.postgresql.org/docs/17/datatype-oid.html>
--
-- PostgreSQL @oid@ type wrapper around 'Word32'.
newtype Oid = Oid Word32
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Oid)

instance Primitive Oid where
  typeName = Tagged "oid"
  baseOid = Tagged 26
  arrayOid = Tagged 1028
  binaryEncoder (Oid x) = Write.bWord32 x
  binaryDecoder = PeekyBlinders.statically (Right . Oid <$> PeekyBlinders.beUnsignedInt4)
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
  from = Oid

-- | Direct conversion from PostgreSQL Oid to 'Word32'.
-- This is a total conversion as it always succeeds.
instance IsMany Oid Word32 where
  from (Oid w) = w

-- | Bidirectional conversion between 'Word32' and PostgreSQL Oid.
instance Is Word32 Oid

instance Is Oid Word32
