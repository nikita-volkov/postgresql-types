module PrimitiveLayer.Types.Int8 (Int8) where

import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (Int8)
import PrimitiveLayer.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @int8@ type. 8-byte signed integer.
--
-- Range: @-9223372036854775808@ to @+9223372036854775807@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-numeric.html#DATATYPE-INT).
newtype Int8 = Int8 Int64
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsPrimitive Int8)

instance IsPrimitive Int8 where
  typeName = Tagged "int8"
  baseOid = Tagged 20
  arrayOid = Tagged 1016
  binaryEncoder (Int8 x) = Write.bInt64 x
  binaryDecoder = PtrPeeker.fixed (Right . Int8 <$> PtrPeeker.beSignedInt8)
  textualEncoder (Int8 x) = TextBuilder.decimal x

-- | Mapping to @int8range@ type.
instance IsRangeElement Int8 where
  rangeTypeName = Tagged "int8range"
  rangeOid = Tagged 3926
  rangeArrayOid = Tagged 3927

-- | Mapping to @int8multirange@ type.
instance IsMultirangeElement Int8 where
  multirangeTypeName = Tagged "int8multirange"
  multirangeOid = Tagged 4536
  multirangeArrayOid = Tagged 6157

-- | Direct conversion from 'Int64'.
-- This is always safe since both types represent 64-bit signed integers identically.
instance IsSome Int64 Int8 where
  to (Int8 i) = i
  maybeFrom = Just . Int8

-- | Direct conversion from PostgreSQL Int8 to 'Int64'.
-- This is always safe since both types represent 64-bit signed integers identically.
instance IsSome Int8 Int64 where
  to i = Int8 i
  maybeFrom (Int8 i) = Just i

-- | Direct conversion from 'Int64'.
-- This is a total conversion as it always succeeds.
instance IsMany Int64 Int8 where
  onfrom = Int8

-- | Direct conversion from PostgreSQL Int8 to 'Int64'.
-- This is a total conversion as it always succeeds.
instance IsMany Int8 Int64 where
  onfrom (Int8 i) = i

-- | Bidirectional conversion between 'Int64' and PostgreSQL Int8.
instance Is Int64 Int8

instance Is Int8 Int64
