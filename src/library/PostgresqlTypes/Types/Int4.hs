module PostgresqlTypes.Types.Int4 (Int4) where

import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @int4@ type. 4-byte signed integer.
--
-- Range: @-2147483648@ to @+2147483647@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-numeric.html#DATATYPE-INT).
newtype Int4 = Int4 Int32
  deriving newtype (Eq, Ord, Arbitrary, Enum, Bounded)
  deriving (Show) via (ViaIsStandardType Int4)

instance IsStandardType Int4 where
  typeName = Tagged "int4"
  baseOid = Tagged 23
  arrayOid = Tagged 1007
  binaryEncoder (Int4 x) = Write.bInt32 x
  binaryDecoder = PtrPeeker.fixed (Right . Int4 <$> PtrPeeker.beSignedInt4)
  textualEncoder (Int4 x) = TextBuilder.decimal x

-- | Mapping to @int4range@ type.
instance IsRangeElement Int4 where
  rangeTypeName = Tagged "int4range"
  rangeOid = Tagged 3904
  rangeArrayOid = Tagged 3905

-- | Mapping to @int4multirange@ type.
instance IsMultirangeElement Int4 where
  multirangeTypeName = Tagged "int4multirange"
  multirangeOid = Tagged 4451
  multirangeArrayOid = Tagged 6150

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
  onfrom = Int4

-- | Direct conversion from PostgreSQL Int4 to 'Int32'.
-- This is a total conversion as it always succeeds.
instance IsMany Int4 Int32 where
  onfrom (Int4 i) = i

-- | Bidirectional conversion between 'Int32' and PostgreSQL Int4.
instance Is Int32 Int4

instance Is Int4 Int32
