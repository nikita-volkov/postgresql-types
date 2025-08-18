-- | PostgreSQL @float8@ type.
-- Represents a 64-bit floating point number in PostgreSQL.
module PrimitiveLayer.Primitives.Float8 (Float8) where

import Data.Bits
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import qualified PeekyBlinders
import PrimitiveLayer.Algebra
import PrimitiveLayer.Prelude
import qualified PtrPoker.Write as Write
import qualified TextBuilder

-- | PostgreSQL @float8@ type wrapper around 'Double'.
newtype Float8 = Float8 Double
  deriving newtype (Eq, Ord, Arbitrary)
  deriving (Show) via (ViaPrimitive Float8)

instance Primitive Float8 where
  typeName = Tagged "float8"
  baseOid = Tagged 701
  arrayOid = Tagged 1022
  binaryEncoder (Float8 x) = Write.bWord64 (castDoubleToWord64 x)
  binaryDecoder = PeekyBlinders.statically (Right . Float8 . castWord64ToDouble <$> PeekyBlinders.beUnsignedInt8)
  textualEncoder (Float8 x) = TextBuilder.string (show x)

-- | Direct conversion from 'Double'.
-- This is always safe since both types represent 64-bit floating point numbers identically.
instance IsSome Double Float8 where
  to (Float8 d) = d
  maybeFrom = Just . Float8

-- | Direct conversion from PostgreSQL Float8 to 'Double'.
-- This is always safe since both types represent 64-bit floating point numbers identically.
instance IsSome Float8 Double where
  to d = Float8 d
  maybeFrom (Float8 d) = Just d

-- | Direct conversion from 'Double'.
-- This is a total conversion as it always succeeds.
instance IsMany Double Float8 where
  from = Float8

-- | Direct conversion from PostgreSQL Float8 to 'Double'.
-- This is a total conversion as it always succeeds.
instance IsMany Float8 Double where
  from (Float8 d) = d

-- | Bidirectional conversion between 'Double' and PostgreSQL Float8.
instance Is Double Float8
instance Is Float8 Double
