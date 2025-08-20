module PrimitiveLayer.Primitives.Int8 (Int8) where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude hiding (Int8)
import PrimitiveLayer.Vias
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | @int8@. 8-byte signed integer. Range: -9223372036854775808 to +9223372036854775807.
--
-- <https://www.postgresql.org/docs/17/datatype-numeric.html#DATATYPE-INT>
--
-- PostgreSQL @int8@ type wrapper around 'Int64'.
newtype Int8 = Int8 Int64
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Int8)

instance Primitive Int8 where
  typeName = Tagged "int8"
  baseOid = Tagged 20
  arrayOid = Tagged 1016
  binaryEncoder (Int8 x) = Write.bInt64 x
  binaryDecoder = PeekyBlinders.statically (Right . Int8 <$> PeekyBlinders.beSignedInt8)
  textualEncoder (Int8 x) = TextBuilder.decimal x

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
  from = Int8

-- | Direct conversion from PostgreSQL Int8 to 'Int64'.
-- This is a total conversion as it always succeeds.
instance IsMany Int8 Int64 where
  from (Int8 i) = i

-- | Bidirectional conversion between 'Int64' and PostgreSQL Int8.
instance Is Int64 Int8

instance Is Int8 Int64
