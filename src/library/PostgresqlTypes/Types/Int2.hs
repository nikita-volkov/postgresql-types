module PostgresqlTypes.Types.Int2 (Int2) where

import PostgresqlTypes.Algebra
import PostgresqlTypes.Prelude
import PostgresqlTypes.Via
import qualified PtrPeeker
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @int2@ type. 2-byte signed integer.
--
-- Range: @-32768@ to @+32767@.
--
-- [PostgreSQL docs](https://www.postgresql.org/docs/17/datatype-numeric.html#DATATYPE-INT).
newtype Int2 = Int2 Int16
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaIsPrimitive Int2)

instance IsPrimitive Int2 where
  typeName = Tagged "int2"
  baseOid = Tagged 21
  arrayOid = Tagged 1005
  binaryEncoder (Int2 x) = Write.bInt16 x
  binaryDecoder = PtrPeeker.fixed (Right . Int2 <$> PtrPeeker.beSignedInt2)
  textualEncoder (Int2 x) = TextBuilder.decimal x

-- | Direct conversion from 'Int16'.
-- This is always safe since both types represent 16-bit signed integers identically.
instance IsSome Int16 Int2 where
  to (Int2 i) = i
  maybeFrom = Just . Int2

-- | Direct conversion from PostgreSQL Int2 to 'Int16'.
-- This is always safe since both types represent 16-bit signed integers identically.
instance IsSome Int2 Int16 where
  to i = Int2 i
  maybeFrom (Int2 i) = Just i

-- | Direct conversion from 'Int16'.
-- This is a total conversion as it always succeeds.
instance IsMany Int16 Int2 where
  onfrom = Int2

-- | Direct conversion from PostgreSQL Int2 to 'Int16'.
-- This is a total conversion as it always succeeds.
instance IsMany Int2 Int16 where
  onfrom (Int2 i) = i

-- | Bidirectional conversion between 'Int16' and PostgreSQL Int2.
instance Is Int16 Int2

instance Is Int2 Int16
