-- | PostgreSQL @int2@ type.
-- Represents a 16-bit signed integer in PostgreSQL.
module PrimitiveLayer.Primitives.Int2 (Int2 (..)) where

import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @int2@ type wrapper around 'Int16'.
newtype Int2 = Int2 Int16
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Int2)

instance Primitive Int2 where
  typeName = Tagged "int2"
  baseOid = Tagged 21
  arrayOid = Tagged 1005
  binaryEncoder (Int2 x) = Write.bInt16 x
  binaryDecoder = PeekyBlinders.statically (Right . Int2 <$> PeekyBlinders.beSignedInt2)
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
  from = Int2

-- | Direct conversion from PostgreSQL Int2 to 'Int16'.
-- This is a total conversion as it always succeeds.
instance IsMany Int2 Int16 where
  from (Int2 i) = i

-- | Bidirectional conversion between 'Int16' and PostgreSQL Int2.
instance Is Int16 Int2
instance Is Int2 Int16
