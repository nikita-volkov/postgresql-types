module PrimitiveLayer.Types.Int4 (Int4) where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import PrimitiveLayer.Via
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @int4@ type. 4-byte signed integer.
--
-- Range: @-2147483648@ to @+2147483647@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-numeric.html#DATATYPE-INT)
newtype Int4 = Int4 Int32
  deriving newtype (Eq, Ord, Arbitrary, Enum, Bounded)
  deriving (Show) via (ViaPrimitive Int4)

instance Mapping Int4 where
  typeName = Tagged "int4"
  baseOid = Tagged 23
  arrayOid = Tagged 1007
  binaryEncoder (Int4 x) = Write.bInt32 x
  binaryDecoder = PeekyBlinders.statically (Right . Int4 <$> PeekyBlinders.beSignedInt4)
  textualEncoder (Int4 x) = TextBuilder.decimal x

instance RangeMapping Int4 where
  rangeTypeName = Tagged "int4range"
  rangeOid = Tagged 3904
  rangeArrayOid = Tagged 3905

-- | Direct conversion from 'Int32'.
-- This is always safe since both types represent 32-bit signed integers identically.
instance IsSome Int32 Int4 where
  to (Int4 i) = i
  maybeFrom = Just . Int4

-- | Direct conversion from PostgreSQL Int4 to 'Int32'.
-- This is always safe since both types represent 32-bit signed integers identically.
instance IsSome Int4 Int32 where
  to i = Int4 i
  maybeFrom (Int4 i) = Just i

-- | Direct conversion from 'Int32'.
-- This is a total conversion as it always succeeds.
instance IsMany Int32 Int4 where
  from = Int4

-- | Direct conversion from PostgreSQL Int4 to 'Int32'.
-- This is a total conversion as it always succeeds.
instance IsMany Int4 Int32 where
  from (Int4 i) = i

-- | Bidirectional conversion between 'Int32' and PostgreSQL Int4.
instance Is Int32 Int4

instance Is Int4 Int32
